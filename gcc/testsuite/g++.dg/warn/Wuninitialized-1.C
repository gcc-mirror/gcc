/* { dg-options "-O2 -Wuninitialized" } */

struct Empty { Empty() {} }; /* { dg-bogus "uninitialized" } */
struct Other {
    Other(const Empty& e_) : e(e_) {}
    Empty e;
};
void bar(Other&);
void foo()
{
  Empty e;
  Other o(e);
  bar(o);
}

