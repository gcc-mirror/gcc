/* Origin: Joseph Myers <jsm28@cam.ac.uk>.

   This tests for inconsistency in whether wide STRING_CSTs use the host
   or the target endianness.  */

extern void exit (int);
extern void abort (void);

int
main (void)
{
  if (L"a" "b"[1] != L'b')
    abort ();
  exit (0);
}
