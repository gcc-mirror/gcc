/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-vrp1-stats -fdump-tree-dom2-stats" } */

void foo();
void bla();
void bar();

/* This is a TODO -- it is correct to thread both entry and latch edge through
   the header, but we do not handle this case yet.  */

void dont_thread_1 (void)
{
  int i = 0;
  int first = 1;

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

/* In this case we can thread if and only if we thread through the
   header and around the latch.  If one is done without the other, then
   we create an irreducible CFG. */
/* { dg-final { scan-tree-dump-not "Jumps threaded" "vrp1"} } */
/* { dg-final { scan-tree-dump-not "Jumps threaded" "dom2"} } */
