/* PR c++/102640 */
/* { dg-do compile } */
/* { dg-additional-options "-fdump-tree-gimple -fdump-tree-omplower" } */
/* Verify var[123] are mapped without any copying, because they are
   mentioned in declare target directive to clauses.  */
/* { dg-final { scan-tree-dump-not "firstprivate\\\(var\[123]\\\)" "gimple" } } */
/* { dg-final { scan-tree-dump-not ".omp_data_arr.\[0-9]*.var" "omplower" } } */
/* { dg-final { scan-tree-dump-not ".omp_data_i->var" "omplower" } } */

void
foo (void)
{
  extern int var1;
  #pragma omp declare target to (var1)

  #pragma omp target
  var1++;
}

int
bar (int x)
{
  extern int var2;
  #pragma omp declare target to (var2)
  if (x)
    return var2;
  #pragma omp target
  var2++;
  return -1;
}
#pragma omp declare target to (bar)

#pragma omp declare target
int
baz (int x)
{
  extern int var3;
  if (x)
    return var3;
  #pragma omp target
  var3++;
  return -1;
}
#pragma omp end declare target
