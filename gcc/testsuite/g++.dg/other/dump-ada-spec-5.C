/* { dg-do compile } */
/* { dg-options "-fdump-ada-spec" } */

namespace foo
{
  int bar = 0;
}

namespace bar = foo;

/* { dg-final { cleanup-ada-spec } } */
