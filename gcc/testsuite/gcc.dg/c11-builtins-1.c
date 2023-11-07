/* Test C11 built-in functions: test functions new in C23 are not
   declared as built-in for C11.  */
/* { dg-do compile } */
/* { dg-options "-std=c11" } */

int exp10 (void);
int exp10f (void);
int exp10l (void);
int fabsd32 (void);
int fabsd64 (void);
int fabsd128 (void);
int nand32 (void);
int nand64 (void);
int nand128 (void);
int roundeven (void);
int roundevenf (void);
int roundevenl (void);
int strdup (void);
int strndup (void);
