// PR c++/47703

typedef void (*pfn)(int &);

struct A
{
  operator pfn() { return 0; }
};

void f()
{
  const int i = 42;
  A()(i);			// { dg-message "<conversion>" }
  // { dg-error "qualifiers" "" { target *-*-* } 13 }
}

// { dg-prune-output "no match" }
