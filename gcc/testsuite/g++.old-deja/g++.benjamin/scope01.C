// Build don't link:
// 980604 bkoz
// 3.4.5 Class member access p 4
// nested and non-nested calls, no dtors

struct L { 
  int ii; 
  void foo(int a) {++a;}
  struct Linner {
    int ii_inner; 
    void foo_inner(int b) {++b;}
  };
};
class A : public L {};
class B : public L {};
class C : public A, public B {};


void foo() {
  // straight call
  C x;
  x.A::ii = 5;			// ERROR - L is ambiguous base
  x.A::foo(x.A::ii);		// ERROR - L is ambiguous base
  
  // 5.1 Primary expressions
  // p 8 
  // a nested name specifier that names a class,
  // optionally followed by the keyword template and then followd by
  // the name of a member of either that class or one of its base
  // classes is a qualified-id.  (3.4.3.1 describes their lookup.) 

  // 5.2.5 Class memember access 

  // p 3 if E1 has the type 'pointer to class X' then 
  // E1->E2 == (*(E1)).E32
  // E1 == object-expression
  // E2 == id-expression
  // thus everything gets converted to the "." notation

  // p 2
  // the id-expression shall name a member of the class
  // (object-expression) or of one of its base classes.

  // p4 if E2 is a nested type (of the object-expression), tye
  // expression E1.E2 is ill formed.

  // try 1 nested call - ERROR
#if 0
  C x2;
  x2.A::L::Linner::ii_inner = 6; //ERROR violates p2, does not name member of C
  x2.A::L::Linner::foo_inner(x2.A::L::Linner::ii_inner);
#endif

  //try2: scoped method call  -edg +acc +g++
#if 1
  C::A::Linner x2;
  x2.A::Linner::ii_inner = 6;
  x2.A::Linner::foo_inner(x2.A::Linner::ii_inner);
#endif

  //try 3: non-scoped method call  -edg +acc +g++
#if 0
  C::A::L::Linner x3;
  x3.ii_inner = 6;
  x3.foo_inner(x3.ii_inner);
#endif
}




