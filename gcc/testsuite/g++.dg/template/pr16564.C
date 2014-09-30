// { dg-do compile }
template<typename> struct A
{
  A<A> a; /* { dg-error "depth" } */
  A() {}
};

A<int> a;

// { dg-prune-output "compilation terminated" }
