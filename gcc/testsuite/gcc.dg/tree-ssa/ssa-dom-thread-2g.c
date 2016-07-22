/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-dom2-details" } */

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

/* This one can only be threaded if both paths to the
   conditional inside the loop are threaded at the same
   time.  Else we potentially end up with irreducible
   loops.  */
/* { dg-final { scan-tree-dump-not "IRREDUCIBLE_LOOP" "dom2" } } */
