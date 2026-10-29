/* { dg-do compile } */
/* { dg-options "-O2" } */

/* Check that vector type alignment is capped at 256 bytes.  */

typedef int v32si __attribute__ ((vector_size (128)));
typedef double v32df __attribute__ ((vector_size (256)));
typedef double v64df __attribute__ ((vector_size (512)));
typedef double v128df __attribute__ ((vector_size (1024)));

v32si a;
v32df b;
v64df c;
v128df d;

/* { dg-final { scan-assembler-times "\\.align\t128\n" 1 } } */
/* { dg-final { scan-assembler-times "\\.align\t256\n" 3 } } */
