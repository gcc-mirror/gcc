// PR rtl-optimization/56999
// { dg-do run }
// { dg-options "-O2" }
// { dg-additional-options "-fpic" { target fpic } }
// { dg-additional-options "-march=i686 -mtune=atom" { target ia32 } }
// { dg-require-visibility "" }

extern "C" void abort (void);
extern "C" void exit (int);
volatile bool do_exit = true;
struct JSScript;
struct JITScript { int i; };
#pragma GCC visibility push(hidden)
typedef struct JSCompartment JSCompartment;
typedef struct JSContext JSContext;
namespace js
{
  struct ContextFriendFields
  {
    JSCompartment *compartment;
  };
  struct TempAllocPolicy
  {
  };
  template <class T>
  struct Vector
  {
    T *mBegin;
    T *begin () { return mBegin; }
    T & operator[] (unsigned i) { return begin ()[i]; }
    template <class U>
    __attribute__((noinline, noclone))
    bool append (U) { asm volatile ("" : : : "memory"); if (do_exit) abort (); return false; }
  };
  namespace types
  {
    struct TypeCompartment;
  }
  namespace mjit
  {
  }
  namespace ion
  {
    struct IonScript;
  }
  namespace types
  {
    struct CompilerOutput
    {
      enum Kind { MethodJIT, ParallelIon };
      JSScript *script;
      unsigned kindInt : 2;
      bool constructing : 1;
      bool barriers : 1;
      bool pendingRecompilation : 1;
      Kind kind () const { return static_cast <Kind> (kindInt); }
      bool isValid () const;
    };
    struct RecompileInfo
    {
      unsigned outputIndex;
      CompilerOutput *compilerOutput (TypeCompartment & types) const;
      CompilerOutput *compilerOutput (JSContext *cx) const;
    };
    struct TypeCompartment
    {
      Vector <CompilerOutput> *constrainedOutputs;
      Vector <RecompileInfo> *pendingRecompiles;
      void addPendingRecompile (JSContext *cx, const RecompileInfo & info);
    };
  }
}
struct JSScript
{
  struct JITScriptHandle
  {
    static volatile JITScript *UNJITTABLE __attribute__((visibility ("default")));
    JITScript *value;
    bool isValid () { return value != UNJITTABLE; }
    JITScript *getValid () { return value; }
  };
  struct JITScriptSet
  {
    JITScriptHandle jitHandleNormal, jitHandleNormalBarriered;
    JITScriptHandle jitHandleCtor, jitHandleCtorBarriered;
    JITScriptHandle jitNull1, jitNull2;
  };
  JITScriptSet *mJITInfo;
  void *ion;
  JITScriptHandle *jitHandle (bool constructing, bool barriers)
  {
    return constructing ? (barriers ? &mJITInfo->jitHandleCtorBarriered
				    : &mJITInfo->jitHandleCtor)
			: (barriers ? &mJITInfo->jitHandleNormalBarriered
				    : &mJITInfo->jitHandleNormal);
  }
  JITScript *getJIT (bool constructing, bool barriers)
  {
    JITScriptHandle *jith = jitHandle (constructing, barriers);
    return jith->isValid () ? jith->getValid () : __null;
  }
};
struct JSContext : js::ContextFriendFields
{
};
namespace js
{
  __attribute__((noinline, noclone))
  void CancelOffThreadIonCompile (JSCompartment *, JSScript *)
  {
    if (do_exit)
      exit (0);
  }
}
struct JSCompartment
{
  js::types::TypeCompartment types;
};
namespace js
{
  namespace types
  {
    inline bool CompilerOutput::isValid () const
    {
      if (!script)
	return false;
      switch (kind ())
	{
	case MethodJIT:
	  {
	    JITScript *jit = script->getJIT (constructing, barriers);
	    if (!jit)
	      return false;
	  }
	case ParallelIon:
	  return true;
	}
      return false;
    }
    inline CompilerOutput *RecompileInfo::compilerOutput (TypeCompartment & types) const
    {
      return &(*types.constrainedOutputs)[outputIndex];
    }
    inline CompilerOutput *RecompileInfo::compilerOutput (JSContext *cx) const
    {
      return compilerOutput (cx->compartment->types);
    }
  }
}
using namespace js::types;
__attribute__((noinline, noclone)) void
TypeCompartment::addPendingRecompile (JSContext *cx, const RecompileInfo & info)
{
  CompilerOutput *co = info.compilerOutput (cx);
  if (co->pendingRecompilation)
    if (co->isValid ())
      CancelOffThreadIonCompile (cx->compartment, co->script);
  if (co->isValid ())
    pendingRecompiles->append (info);
}
volatile JITScript *JSScript::JITScriptHandle::UNJITTABLE;
#pragma GCC visibility pop
int
main ()
{
  JSContext cx;
  JSCompartment com;
  RecompileInfo info;
  cx.compartment = &com;
  info.outputIndex = 0;
  js::Vector<CompilerOutput> v;
  JITScript js;
  JSScript::JITScriptSet set;
  __builtin_memset (&set, 0, sizeof set);
  set.jitHandleCtor.value = &js;
  JSScript s;
  s.mJITInfo = &set;
  CompilerOutput co;
  co.kindInt = 0;
  co.constructing = true;
  co.barriers = false;
  co.pendingRecompilation = true;
  co.script = &s;
  v.mBegin = &co;
  com.types.constrainedOutputs = &v;
  com.types.pendingRecompiles = __null;
  com.types.addPendingRecompile (&cx, info);
  abort ();
}
