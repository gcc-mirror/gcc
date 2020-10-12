/* PR target/60693 */
/* { dg-do compile } */
/* { dg-options "-O0" } */

void bar (char *);

void
foo (void)
{
  char buf[4096];
  __builtin_memcpy (buf, (void *) 0x8000, 4096);
  bar (buf);
}

/* Reading from a constant address might triggers:
   { dg-prune-output "\\\[-Wstringop-overread" } */
