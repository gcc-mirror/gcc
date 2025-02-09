/* { dg-do compile } */
/* { dg-additional-options "-foffload=disable -fdump-tree-original" } */

/* Check for elision of preprocessed code in a begin/end declare variant
   construct when it can be determined at parse time that the selector
   can never match.  */

int foobar (int x, int y)
{
  return x * y;
}

int baz (int x)
{
  return x;
}

#pragma omp begin declare variant match (implementation={vendor("acme")}) /* { dg-warning "unknown property" } */
int foobar (int x, int y)
{
  random junk that would ordinarily cause a parse error;
  return x + y;
}
#pragma omp end declare variant

#pragma omp begin declare variant match (device={kind(fpga)})
int foobar (int x, int y)
{
  random junk that would ordinarily cause a parse error;
  return x + y;
}
#pragma omp end declare variant

/* Per the OpenMP specification, elision only happens when the implementation
   or device selectors cannot match; the user/condition selector doesn't
   matter for this.  */
#pragma omp begin declare variant match (user={condition (0)})
int foobar (int x, int y)
{
  return x + y;
}
#pragma omp end declare variant

/* Check that we're finding the right "omp end declare variant" when
   constructs are nested.  */
#pragma omp begin declare variant match (implementation={vendor("acme")})  /* { dg-warning "unknown property" } */
  #pragma omp begin declare variant match (device={kind(fpga)})
  int baz (int x)
  {
    random junk that would ordinarily cause a parse error;
    return x + 1;
  }
  #pragma omp end declare variant
  #pragma omp begin declare variant match (device={kind(host)})
  int baz (int x)
  {
    random junk that would ordinarily cause a parse error;
    return x + 2;
  }
  #pragma omp end declare variant
#pragma omp end declare variant

/* { dg-final { scan-tree-dump-times "foobar.ompvariant" 1 "original" } } */
/* { dg-final { scan-tree-dump-not "baz.ompvariant" "original" } } */


