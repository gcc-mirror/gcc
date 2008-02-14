// { dg-options "-Wreorder -W" }

struct S {
  S ();
};

struct T {
  T ();
};

struct U : virtual public S, virtual public T {
  U () : T (), S () {}     // { dg-warning "" }
  U (const U&) : S () {}
};
