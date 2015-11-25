/* { dg-do compile } */ 
/* { dg-options "-O2 -fdump-tree-vrp1-stats -fdump-tree-dom2-stats" } */

void foo();
void bla();
void bar();

/* Avoid threading in this case, in order to avoid creating loop with
   multiple entries.  */

void dont_thread_4 (int a, int nfirst)
{
  int i = 0;
  int first;

  if (a)
    first = 0;
  else
    first = 1;

  do
    {
      if (first)
	foo ();
      else
	bar ();

      first = nfirst;
      bla ();
    } while (i++ < 100);
}

/* { dg-final { scan-tree-dump-not "Jumps threaded" "vrp1"} } */
/* { dg-final { scan-tree-dump-not "Jumps threaded" "dom2"} } */
