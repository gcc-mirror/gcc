/* { dg-do compile } */
/* { dg-require-effective-target vect_long } */
/* { dg-additional-options "-mavx2" { target avx2 } } */

void foo (long *p, long *q, long *r)
{
  long tem0 = r[0];
  long tem1 = r[1];
  long tem2 = r[2];
  long tem3 = r[3];
  tem0 = tem0 + 1;
  tem1 = tem1 + 2;
  tem2 = tem2 + 3;
  tem3 = tem3 + 4;
  p[0] = tem2;
  p[1] = tem3;
  q[0] = tem0;
  q[1] = tem1;
  q[2] = tem2;
  q[3] = tem3;
}

/* { dg-final { scan-tree-dump "CSEd node\[^\n\r\]*highpart" "slp2" } } */
/* { dg-final { scan-tree-dump "BIT_FIELD_REF" "slp2" { target avx2 } } } */
