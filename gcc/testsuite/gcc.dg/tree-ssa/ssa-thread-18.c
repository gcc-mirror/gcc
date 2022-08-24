/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-threadfull1-stats" } */

void foo (int nest, int print_nest)
{
  _Bool t0 = nest != 0;
  _Bool t1 = nest == print_nest;
  _Bool t2 = t0 & t1;
  if (t2)
    __builtin_puts ("x");
  nest++;
  if (nest > 2)
    __builtin_abort ();
  if (print_nest == nest)
    __builtin_puts ("y");
}

/* We should be able to thread (t2) to !(print_nest == nest) using the
   nest == print_nest relation implied by the entry block.  */
/* { dg-final { scan-tree-dump "Jumps threaded: 1" "threadfull1" } } */
