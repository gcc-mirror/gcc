// PR middle-end/15054
// This used to abort due to overlapping stack temporaries.

// { dg-do run }
// { dg-options "-O" }

extern "C" void abort (void);

struct pointer
{
  void* ptr;

  pointer(void* x = 0) : ptr(x) {}
  pointer(const pointer& x) : ptr(x.ptr) {}
};

struct element
{
  int canary;

  element() : canary(123) { }
  ~element() { pointer(); if (canary != 123) abort (); }
};

inline pointer
insert(const element& x)
{
  return pointer(new element(x));
}

int
main (void)
{
  insert(element());
  return 0;
}
