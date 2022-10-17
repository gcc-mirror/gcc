/* { dg-do run { target { power10_hw } } } */
/* { dg-require-effective-target rop_ok } */
/* { dg-options "-O2 -mdejagnu-cpu=power10 -mrop-protect" } */

/* Verify that ROP-protect instructions execute correctly when a
   call is present.  */

void __attribute__((noipa)) foo ()
{
  asm ("");
}

int main ()
{
  foo ();
  return 0;
}

