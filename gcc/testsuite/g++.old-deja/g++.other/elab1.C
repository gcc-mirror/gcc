// { dg-do assemble  }
typedef struct {} S;

S s1;
struct S* s2; // { dg-error "" } S is a typedef name

template <class T>
struct X {
  friend class T; // { dg-error "" } T is a template type parameter
};
