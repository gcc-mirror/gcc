typedef struct {} S;

S s1;
struct S* s2; // ERROR - S is a typedef name

template <class T>
struct X {
  friend class T; // ERROR - T is a template type parameter
};
