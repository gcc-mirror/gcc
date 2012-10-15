// PR c++/20763
// { dg-do compile { target c++11 } }

typedef void *voidp;

struct S
{
  char a;
  voidp b [[gnu::aligned (16)]];
};

struct T
{
  char a;
  void * b [[gnu::aligned (16)]];
};

static_assert (sizeof (S) == sizeof (T),
	       "struct S and T should have the same size");

static_assert (sizeof (S) == 32, "sizeof (S) == 8 + 16 + 8");
