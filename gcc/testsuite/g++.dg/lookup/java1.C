// { dg-do compile }
// { dg-options "-fdollars-in-identifiers" }
// Origin: Giovanni Bajo <giovannibajo at libero dot it>
// Make sure that Java special functions can be called correctly.

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
 * Step 1: no declarations. A declaration for _Jv_Throw is created.
 */

void Bar1(void)
{
  Foo* f = new java::lang::Foo;   // { dg-error "call to Java constructor" }
  throw (f);
}


/*
 * Step 2: constructor declaration
 */

extern "C" jobject _Jv_AllocObject (jclass) __attribute__((__malloc__));

void Bar2(void)
{
  Foo* f = new java::lang::Foo; 
  throw (f);  
}


/*
 * Step 3: overloads
 */

jobject _Jv_AllocObject (jclass, jint, float) __attribute__((__malloc__));
void _Jv_Throw (int, float) __attribute__ ((__noreturn__));

void Bar3(void)
{
  Foo* f = new java::lang::Foo;	  // { dg-error "should never be overloaded" }
  throw (f);			  // { dg-error "should never be overloaded" }
}
