/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-vrp1-stats -fdump-tree-dom2-stats" } */

void foo();
void bla();
void bar();

/* Avoid threading in the following case, to prevent creating subloops.  */

void dont_thread_2 (int first)
{
  int i = 0;

  do
    {
      bla ();
      bla ();
      bla ();
      if (first)
	foo ();
      else
	bar ();

      first = 0;
      bla ();
    } while (i++ < 100);
}

/* Peeling off the first iteration would make threading through
   the loop latch safe, but we don't do that currently.  */
/* { dg-final { scan-tree-dump-not "Jumps threaded" "vrp1"} } */
/* { dg-final { scan-tree-dump-not "Jumps threaded" "dom2"} } */
