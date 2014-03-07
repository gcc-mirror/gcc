// PR c++/50500
// { dg-do compile { target c++11 } }

// If a class declares move operations, the implicitly declared copy
// operations are deleted.
struct A
{
  A();
  A(A&&);
  A& operator=(A&&);
};

// But they can still be explicitly defaulted.
struct B
{
  B();
  B(B&&);
  B(const B&) = default;
  B& operator=(B&&);
  B& operator=(const B&) = default;
};

struct C
{
  C();
  C(C&&);
};

struct D
{
  D();
  D& operator=(D&&);
};

int main()
{
  A a;
  A a2 (a);			// { dg-error "deleted" }
  a2 = a;			// { dg-error "deleted" }

  B b;
  B b2 (b);
  b2 = b;

  C c;
  C c2(c);			// { dg-error "deleted" }
  c2 = c;			// { dg-error "deleted" }

  D d;
  D d2(d);			// { dg-error "deleted" }
  d2 = d;			// { dg-error "deleted" }
}

// { dg-prune-output "because" }
