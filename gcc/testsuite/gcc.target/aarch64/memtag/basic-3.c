/* { dg-do compile } */
/* { dg-additional-options "-O2" } */

int use (int *x);

void foo (int n)
{
 int a, b, c;
 use (&a);
 use (&b);
 use (&c);
}

/* 3 stack vars need 48 bytes (16 bytes granule x 3).  Each granule holds the
   local variable stack address (8 bytes), and a padding (8 bytes).  The rest
   of the stack holds LR and temporary varaibles (i.e., x19, x20, and x29).

   Expected: 3 stg to tag, 1 st2g + 1 stg to untag.  */

/* { dg-final { scan-assembler-times {\tirg\t} 1 } } */
/* { dg-final { scan-assembler-times {\taddg\t...?} 2 } } */
/* { dg-final { scan-assembler-times {\tsubg\t...?} 3 } } */
/* { dg-final { scan-assembler-times {\tstg\t...?, \[sp, 48\]\n} 1 } } */
/* { dg-final { scan-assembler-times {\tst2g\t...?, \[sp, 32\]\n} 1 } } */
/* { dg-final { scan-assembler-times {\tstg\t...?, \[sp, 32\]\n} 1 } } */
/* { dg-final { scan-assembler-times {\tstg\t...?, \[sp, 64\]\n} 2 } } */
/* { dg-final { scan-assembler-times {\tstp\tx19, x20, \[sp, 16\]\n} 1 } } */
/* { dg-final { scan-assembler-times {\tstp\tx29, x30, \[sp, -80]!\n} 1 } } */
