/* { dg-do compile } */
/* { dg-options "-Os -fmodulo-sched -fcompare-debug" } */

void
foo (void)
{
  unsigned numlen;
  unsigned foldlen;
  for (; foldlen; foldlen -= numlen)
    foo ();
}
