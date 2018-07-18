// PR c++/81212
// { dg-do compile }
// { dg-options "-Wreturn-type -fsanitize=return" }

struct S
{
  S (void *);
  void *s;
};

S
foo (bool x, void *y)
{
  if (x)
    return S (y);
}	// { dg-warning "control reaches end of non-void function" }
