/* { dg-do compile { target { ! ia32 } } } */
/* { dg-require-effective-target maybe_x32 } */
/* { dg-options "-O2 -mx32 -maddress-mode=short -mmovdir64b" } */
/* { dg-final { scan-assembler "movdir64b\[ \\t\]" } } */

unsigned int w[5] __attribute__((aligned(64)));

void
foo ()
{

   unsigned int array[] = {1, 2, 3, 4, 5};
   __builtin_ia32_movdir64b(w, array);
}

