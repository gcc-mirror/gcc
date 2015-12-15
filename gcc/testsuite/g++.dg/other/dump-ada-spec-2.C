/* { dg-do compile } */
/* { dg-options "-fdump-ada-spec" } */

struct S
{
  int it;
  __extension__ unsigned char data[];
};

/* { dg-final { scan-ada-spec "array \\(0 .. 0\\)" } } */
/* { dg-final { cleanup-ada-spec } } */
