// PR c++/81675
// { dg-do compile }
// { dg-options "-Wall" }

struct S
{
  ~S () __attribute__((noreturn));
  int a;
};

int
foo ()
{
  false ? 5 : S ().a;
}
