/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-ccp1" } */

typedef double v2df __attribute__ ((vector_size (2 * sizeof (double))));

void do_div (v2df *a, v2df *b)
{
  *a = *b / (v2df) { 2.0, 2.0 };
}

/* Since 2.0 has an exact reciprocal, constant folding should multiply *b
   by the reciprocals of the vector elements.  As a result there should be
   one vector multiply and zero divides in the optimized code.  The fold
   does not take place for generic vectors until the first CCP pass.  The
   string " * " occurs 3 times:  one multiply and two indirect parameters.  */

/* { dg-final { scan-tree-dump-times " \\\* " 3 "ccp1" } } */
/* { dg-final { scan-tree-dump-times " / " 0 "ccp1" } } */
/* { dg-final { cleanup-tree-dump "ccp1" } } */
