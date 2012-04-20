/* { dg-do compile } */
/* { dg-options "-O2 -freciprocal-math -fdump-tree-ccp1" } */

typedef double v2df __attribute__ ((vector_size (16)));

void do_div (v2df *a, v2df *b)
{
  *a = *b / (v2df) { 2.0, 3.0 };
}

/* Constant folding should multiply *b by the reciprocals of the
   vector elements.  The fold does not take place for generic
   vectors until the first CCP pass.  The string " * " occurs 3
   times:  one multiply and two indirect parameters.  */

/* { dg-final { scan-tree-dump-times " \\\* " 3 "ccp1" } } */
/* { dg-final { scan-tree-dump-times " / " 0 "ccp1" } } */
/* { dg-final { cleanup-tree-dump "ccp1" } } */
