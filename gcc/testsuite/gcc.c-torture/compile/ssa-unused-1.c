/* This is just to exercise the '__OPTIMIZE__' DejaGnu selector.  */

/* { dg-do "assemble" } */
/* { dg-additional-options "-fdump-tree-ssa" } */

void f()
{
  int unused = 3;
  (void) &unused;
}

/* { dg-final { scan-tree-dump-not {No longer having address taken: unused} ssa { target { ! __OPTIMIZE__ } } } }
   { dg-final { scan-tree-dump-times {No longer having address taken: unused} 1 ssa { target __OPTIMIZE__ } } } */
/* { dg-final { scan-tree-dump-not {Now a gimple register: unused} ssa { target { ! __OPTIMIZE__ } } } }
   { dg-final { scan-tree-dump-times {Now a gimple register: unused} 1 ssa { target __OPTIMIZE__ } } } */
