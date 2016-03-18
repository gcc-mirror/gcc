// PR c++/70267
// { dg-do compile }
// { dg-options "-O2" }

extern "Java"
{
  typedef __java_int jint;
  namespace java
  {
    namespace lang
    {
      class Class;
      class Object;
      class Throwable {};
      class Foo;
    }
  }
} 

typedef struct java::lang::Object * jobject;
typedef struct java::lang::Throwable * jthrowable;
typedef class  java::lang::Class * jclass;

using java::lang::Foo;

class Foo: public java::lang::Throwable
{
  public:static::java::lang::Class class$;
};

extern "C" Foo _Jv_AllocObject (jclass);
extern "C" void _Jv_Throw (jthrowable) __attribute__ ((__noreturn__));

void 
Bar4 (void)
{
  Foo * f = new java::lang::Foo;	// { dg-error "is not a function returning a pointer" }
  throw (f);
}
