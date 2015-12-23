/* { dg-do compile } */
/* { dg-additional-options  "-O2 -fdump-ipa-cgraph" } */

/* RTL-level CSE shouldn't introduce LCO (for the string) into varpool */
char *p;

void foo ()
{
  p = "abc\n";

  while (*p != '\n')
    p++;
}

/* { dg-final { scan-ipa-dump-not "LC0" "cgraph" } } */
