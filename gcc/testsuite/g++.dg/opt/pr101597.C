// PR c++/101597
// { dg-do compile }
// { dg-options "-O2 -Warray-bounds" }

typedef __SIZE_TYPE__ size_t;
struct S { virtual void *foo (size_t) __attribute__((alloc_size (2))); };

int
foo (void *p)
{
  char *q = static_cast<char *> (static_cast<S *> (p)->foo (32));
  return q[64];		// { dg-warning "array subscript 64 is outside array bounds of" }
}
