// DR 1402
// { dg-options -std=c++11 }

template <class T> T&& move(T& t);

struct A
{
  A(const A&);
};

struct B
{
  B(B&&) = delete;		// { dg-prune-output "declared" }
};

struct C			// { dg-error "deleted" }
{
  A a;
  B b;
};

extern C c1;
C c2(move(c1));			// { dg-error "deleted" }
