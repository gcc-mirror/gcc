/* { dg-do compile } */
/* { dg-options "-O2 -mcmodel=small" } */

const double b[4] = {0.2435334343f, 0.2233535343f, 0.4232433f, 0.34343434f};
typedef double v2df __attribute__ ((vector_size (16)));
typedef double v2df __attribute__ ((vector_size (16)));

v2df f (v2df c1, v2df c2)
{
   v2df a1 = *(v2df *)&b[0];
   v2df a2 = *(v2df *)&b[2];
   return (a1 * c1) + (a2 * c2);
}

/* { dg-final { scan-assembler-times "adrp" 1 } } */
