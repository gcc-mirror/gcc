/* ARM and Thumb asm statements should be able to access the constant
   pool.  */
/* { dg-do compile { target arm*-*-* strongarm*-*-* xscale*-*-*} } */
extern unsigned x[];
unsigned *trapTable()
{
  unsigned *i;

  __asm__ volatile("ldr %0,%1" : "=r"(i) : "m"(x[0]));

  return i;
}

