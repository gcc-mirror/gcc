/* { dg-do compile } */
/* { dg-require-effective-target arm_thumb1_ok } */
/* { dg-options "-Os" } */
/* { dg-skip-if "-mpure-code generates an inline multiplication code sequence" { *-*-* } { "-mpure-code" } } */
/* { dg-skip-if "" { ! { arm_thumb1 } } } */

int
mymul3 (int x)
{
  return x * 0x555;
}

/* { dg-final { scan-assembler "muls\[\\t \]*r.,\[\\t \]*r." } } */
