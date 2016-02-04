/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-vrp1-stats -fdump-tree-dom2-stats" } */

void foo();
void bla();
void bar();

void dont_thread_1 (void)
{
  int i = 0;
  int first = 1;

  do
    {
      if (first)
	foo ();
      else
	bar ();

      first = 0;
      bla ();
    } while (i++ < 100);
}

/* { dg-final { scan-tree-dump "Jumps threaded: 2" "vrp1"} } */
/* { dg-final { scan-tree-dump "Jumps threaded: 1" "dom2"} } */
