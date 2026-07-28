/* PR rtl-optimization/126434 */
/* { dg-do compile } */
/* { dg-require-effective-target int128 } */
/* { dg-options "-O1 -favoid-store-forwarding --param=store-forwarding-max-distance=146" } */

/* The store partially covers the load, so the load is kept and its
   zero-extension is re-applied over a SUBREG destination.  */

unsigned x;
__int128 y;

void
foo ()
{
  __builtin_memset (&x, 0, 2);
  y &= x;
}

