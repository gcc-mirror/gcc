/* { dg-do compile } */ 
/* { dg-options "-O2 -fdump-tree-vrp1-stats -fdump-tree-dom1-stats" } */

void foo();
void bla();
void bar();

/* In the following two cases, we should be able to thread edge through
   the loop header.  */

void thread_entry_through_header (void)
{
  int i;

  for (i = 0; i < 170; i++)
    bla ();
}

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

/* This is a TODO -- it is correct to thread both entry and latch edge through
   the header, but we do not handle this case yet.  */

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

/* Avoid threading in the following two cases, to prevent creating subloops.  */

void dont_thread_2 (int first)
{
  int i = 0;

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

void dont_thread_3 (int nfirst)
{
  int i = 0;
  int first = 0;

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

/* { dg-final { scan-tree-dump-times "Jumps threaded: 1" 1 "vrp1"} } */
/* { dg-final { scan-tree-dump-times "Jumps threaded: 2" 0 "vrp1"} } */
/* { dg-final { scan-tree-dump-times "Jumps threaded: 1" 0 "dom1"} } */
/* { dg-final { scan-tree-dump-times "Jumps threaded: 2" 1 "dom1"} } */
/* { dg-final { cleanup-tree-dump "dom1" } } */
/* { dg-final { cleanup-tree-dump "vrp1" } } */
