// { dg-do compile }
// Origin: Giovanni Bajo <giovannibajo at gcc dot gnu dot org>
// DR90: Should the enclosing class be an "associated class" too? 

struct A {
  union U {};
  friend void f(U);
};

struct B {
  struct S {};
  friend void f(S);
};

int main() { 
  A::U    u; 
  f(u);
  B::S    s;
  f(s);
}
