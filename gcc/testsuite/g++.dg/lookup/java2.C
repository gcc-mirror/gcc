// { dg-do compile }
// { dg-options "-fdollars-in-identifiers" }
// Origin: Giovanni Bajo <giovannibajo at libero dot it>
// Make sure that Java special functions can be called correctly.
// (continue from java1.C)

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

typedef struct java::lang::Object* jobject;
typedef struct java::lang::Throwable* jthrowable;
typedef class java::lang::Class* jclass;
using java::lang::Foo;

class Foo : public java::lang::Throwable
{
public:
  static ::java::lang::Class class$;
};

/*
 * Step 4: Manual declaration of _Jv_Throw
 *  This is the last case we need to test. In the other file we're testing
 *  the compiler is able to generate an artifical declaration for this 
 *  function, so we need to test here if it works with a normal declaration.
 */

extern "C" jobject _Jv_AllocObject (jclass, jint) __attribute__((__malloc__));
extern "C" void _Jv_Throw (jthrowable) __attribute__ ((__noreturn__));

void Bar4(void)
{
  Foo* f = new java::lang::Foo;
  throw (f);
}
