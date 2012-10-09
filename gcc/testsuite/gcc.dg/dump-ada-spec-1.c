/* { dg-do compile } */
/* { dg-options "-fdump-ada-spec" } */

struct S
{
  int i;
};

/* { dg-final { scan-ada-spec "type S is record" } } */
/* { dg-final { cleanup-ada-spec } } */
