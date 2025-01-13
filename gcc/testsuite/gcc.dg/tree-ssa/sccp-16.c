/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-cunrolli -fdump-tree-sccp-details" } */

int foo ()
{
  const char *s = "Hello World!";
  int len = 0;
  while (s[len])
    ++len;
  return len;
}

/* For cunrolli the growth is too large, but it should add a canonical IV
   and SCCP peform final value replacement.  */
/* { dg-final { scan-tree-dump "ivtmp\[^\r\n\]*PHI\[^\r\n\]*13" "cunrolli" } } */
/* { dg-final { scan-tree-dump "with expr: 12" "sccp" } } */
