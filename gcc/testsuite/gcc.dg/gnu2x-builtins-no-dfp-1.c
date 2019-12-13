/* Test C2x built-in functions: test DFP built-in functions are not
   available when no DFP support.  Bug 91985.  */
/* { dg-do compile { target { ! dfp } } } */
/* { dg-options "-std=gnu2x" } */

int fabsd32 (void);
int fabsd64 (void);
int fabsd128 (void);
int nand32 (void);
int nand64 (void);
int nand128 (void);

__typeof__ (__builtin_fabsd32 (0)) d32; /* { dg-warning "implicit" } */
__typeof__ (__builtin_fabsd64 (0)) d64; /* { dg-warning "implicit" } */
__typeof__ (__builtin_fabsd128 (0)) d128; /* { dg-warning "implicit" } */
__typeof__ (__builtin_nand32 (0)) d32n; /* { dg-warning "implicit" } */
__typeof__ (__builtin_nand64 (0)) d64n; /* { dg-warning "implicit" } */
__typeof__ (__builtin_nand128 (0)) d128n; /* { dg-warning "implicit" } */
