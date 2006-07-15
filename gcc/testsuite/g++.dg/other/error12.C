//PR c++/28292

extern "Java"
{
  struct A
  {
    void foo(void; // { dg-error "before|incomplete type|invalid use" }
  };
}
