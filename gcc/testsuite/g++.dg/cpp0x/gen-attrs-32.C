// PR c++/35315
// { dg-do compile { target c++11 } }

typedef union { int i; } U [[gnu::transparent_union]]; // { dg-warning "ignored" }

static void foo(U) {}
static void foo(int) {}

void bar()
{
  foo(0);
}

typedef union U1 { int i; } U2 [[gnu::transparent_union]]; // { dg-warning "ignored" }

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
  typedef union [[gnu::transparent_union]]
  {
    int i;
  } B;
};

void foo(A::B b)
{
  b.i;
}
