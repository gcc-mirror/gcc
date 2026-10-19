/* { dg-options "-fpath-coverage" } */
/* { dg-do compile } */

extern void abort (void);

/* We need function calls at specific places to ensure block creation and
   construct the right paths.  The implementation itself is meaningless.  */
unsigned do1 (unsigned n) { return n; }
unsigned do2 (unsigned n) { return n > 0 ? n - 1 : n; }

/* Distilled from basenc.c in GNU Coreutils 9.1.

   This targets a bug that would happen when the exit paths would be the most
   numerous and be the basis for merge.  This could leave non-prime paths in
   the final set of prime paths.  For this specific example it would be the
   last block, just before the function ends, which is clearly not a prime path
   on its own.

   This extra path happened to be filtered when writing the .gcno/.gcda entry,
   so there was a mismatch between the number of paths in the .gcno file and
   what gcov would report (30 paths):

   $ gcov-dump gcov-44.gcno
   11 /gcov-44.gcno:  01000000: 105:FUNCTION [...] `do_decode' gcov-44.c:33:1-53:1
   12 ./gcov-44.gcno: 01410000:   4:BLOCKS 14 blocks
   ...
   38 ./gcov-44.gcno: 01490000: 4:PATHS 29 paths

   BEGIN paths
   summary: 0/29
*/
void
do_decode()
/* END */
{
  unsigned n = do1 (100);
  do
    {
      while (do2 (n) > 1500)
	{
	  if (n > 1000)
	    abort ();
	  if (n > 100)
	    break;
	  n -= 1;
	}

      n -= 1;
    }
  while (do1 (n) > 0);
}

int
main()
{
  do_decode();
  return 0;
}

/* { dg-final { run-gcov prime-paths { --prime-paths gcov-44.c } } } */
