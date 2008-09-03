// PR c++/30293
// PR c++/30294
// { dg-do compile { target { ! { powerpc-ibm-aix* } } } }
// { dg-options "" }

extern "Java" {
typedef __java_byte jbyte;
namespace java {
namespace lang {
  class Object {};
  class Class {};
}
}
typedef struct java::lang::Object* jobject;
typedef java::lang::Class *jclass;
}
extern "C" jobject _Jv_AllocObject (jclass);

extern "Java" {
  struct A { static java::lang::Class class$; };
}

struct B {
  A a;		// { dg-error "has Java class type" }
};

void* operator new (__SIZE_TYPE__, void*) throw();
char buf[1024];

A a;		// { dg-error "not allocated with" }
A b = A ();	// { dg-error "not allocated with" }
A *c = new ((void *) buf) A ();	// { dg-error "using placement new" }
A *d = new A ();
jbyte e = 6;

const A fn1 ()	// { dg-error "return type has Java class type" }
{
  A a;		// { dg-error "not allocated with" }
  return a;
}

A fn2 ()	// { dg-error "return type has Java class type" }
{
  A a;		// { dg-error "not allocated with" }
  return a;
}

A *fn3 ()
{
  return new A ();
}

A &fn4 ()
{
  return *c;
}

jbyte fn5 ()
{
  return 7;
}

void fn6 (A x)	// { dg-error "has Java class type" }
{
}

void fn7 (const A x)	// { dg-error "has Java class type" }
{
}

void fn8 (A *x)
{
  (void) x;
}

void fn9 (jbyte x)
{
  (void) x;
}
