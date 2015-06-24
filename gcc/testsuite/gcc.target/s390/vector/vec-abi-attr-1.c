/* Check calling convention in the vector ABI.  */

/* { dg-do compile { target { s390*-*-* } } } */
/* { dg-options "-O3 -mzarch -march=z13 -mno-vx" } */

/* The function passes arguments whose calling conventions change with
   -mvx/-mno-vx.  In that case GCC has to emit the ABI attribute to
   allow GDB and Binutils to detect this.  */
/* { dg-final { scan-assembler "gnu_attribute 8, 1" } } */

typedef double v2df __attribute__((vector_size(16)));

v2df
add (v2df a, v2df b, v2df c, v2df d,
     v2df e, v2df f, v2df g, v2df h, v2df i)
{
  return a + b + c + d + e + f + g + h + i;
}
