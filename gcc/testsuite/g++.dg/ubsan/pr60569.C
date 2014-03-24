// PR sanitizer/60569
// { dg-do link }
// { dg-require-effective-target lto }
// { dg-options "-fsanitize=undefined -flto" }

struct A
{
  void foo ();
  struct
  {
    int i;
    void bar () { i = 0; }
  } s;
};

void A::foo () { s.bar (); }

int
main ()
{
}
