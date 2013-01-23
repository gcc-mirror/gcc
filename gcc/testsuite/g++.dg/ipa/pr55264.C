/* { dg-do compile } */
/* { dg-options "-O2 -fno-early-inlining -fno-weak"  } */

struct S
{
  S();
  virtual inline void foo ()
  {
    foo();
  }
};

void
B ()
{
  S().foo ();
}
