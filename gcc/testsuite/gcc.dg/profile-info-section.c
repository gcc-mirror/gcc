/* { dg-do compile } */
/* { dg-skip-if "profile-info-section" { powerpc-ibm-aix* } } */
/* { dg-options "-fprofile-arcs -fprofile-info-section -fdump-tree-optimized" } */

int foo()
{
  return 0;
}

int bar()
{
  return 1;
}

int main ()
{
  return foo ();
}

/* { dg-final { scan-tree-dump-not "__gcov_init" "optimized" } } */
/* { dg-final { scan-tree-dump-not "__gcov_exit" "optimized" } } */
/* { dg-final { scan-assembler "\.gcov_info" } } */
