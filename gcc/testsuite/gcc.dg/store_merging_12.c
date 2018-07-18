/* { dg-do compile } */
/* { dg-options "-O2 -Wall" } */

#if __SIZEOF_INT__ == 2
#define int long
#endif

struct S { unsigned int b1:1, b2:1, b3:1, b4:1, b5:1, b6:27; };
void bar (struct S *);
void foo (int x)
{
  struct S s;
  s.b2 = 1; s.b3 = 0; s.b4 = 1; s.b5 = 0; s.b1 = x; s.b6 = x;	/* { dg-bogus "is used uninitialized in this function" } */
  bar (&s);
}
