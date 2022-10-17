/* { dg-lto-do link } */
/* { dg-skip-if "power10 and above only" { ! { power10_ok } } } */
/* { dg-lto-options { "-O2 -mdejagnu-cpu=power8 -mno-power8-fusion -flto -fdump-ipa-inline" } } */

int
foo1 (int *b)
{
  *b += 100;
  return *b;
}

/* { dg-final { scan-wpa-ipa-dump-not "target specific option mismatch" "inline"  } } */
