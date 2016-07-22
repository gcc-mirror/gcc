/* { dg-do compile } */ 
/* { dg-options "-O2 -fdump-tree-thread1-stats -fdump-tree-dom2-stats" } */

void foo();
void bla();

/* In the following case, we should be able to thread edge through
   the loop header.  */

void thread_latch_through_header (void)
{
  int i = 0;
  int first = 1;

  do
    {
      if (first)
	foo ();

      first = 0;
      bla ();
    } while (i++ < 100);
}

/* Threading the latch to a later point in the loop is safe in this
   case.  And we want to thread through the header as well.  These
   are both caught by threading in DOM.  */
/* { dg-final { scan-tree-dump-not "Jumps threaded" "dom2"} } */
/* { dg-final { scan-tree-dump-times "Jumps threaded: 1" 1 "thread1"} } */
