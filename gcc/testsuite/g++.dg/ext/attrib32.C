// PR c++/35315

typedef union { int i; } U __attribute__((transparent_union));

static void foo(U) {}
static void foo(int) {}

void bar()
{
  foo(0);
}

typedef union U1 { int i; } U2 __attribute__((transparent_union)); // { dg-warning "ignored" }

static void foo2(U1) {}		// { dg-error "previously defined" }
static void foo2(U2) {}		// { dg-error "redefinition" }

void bar2(U1 u1, U2 u2)
{
  foo2(u1);
  foo2(u2);
}

// PR c++/36410
struct A
{
  typedef union
  {
    int i;
  } B __attribute__((transparent_union));
};

void foo(A::B b)
{
  b.i;
}
