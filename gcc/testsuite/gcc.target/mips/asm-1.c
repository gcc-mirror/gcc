/* PR target/17565.  GCC used to put the asm into the delay slot
   of the call.  */
/* { dg-do assemble } */

NOMIPS16 int foo (int n)
{
  register int k asm ("$16") = n;
  if (k > 0)
    {
      bar ();
      asm ("li %0,0x12345678" : "=r" (k));
    }
  return k;
}
