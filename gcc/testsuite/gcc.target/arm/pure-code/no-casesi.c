/* { dg-do compile } */
/* { dg-options "-mpure-code" } */
/* { dg-skip-if "" { *-*-* } { "-g" "-fpic" "-fPIC" } { "" } } */

extern int foo (void);
extern int bar (void);
extern int baz (void);
extern int fooz (void);

int caller (unsigned int reg_type)
{
  switch (reg_type)
    {
    case 0x80000000:
      return (int) foo ();

    case 0x80000003:
      return (int) bar ();

    case 0x80000001:
      return (int) baz ();

    case 0x80000004:
      return (int) fooz ();
    }
}

/* { dg-final { scan-assembler-not "\\.(float|l\\?double|\d?byte|short|int|long|quad|word)\\s+\[^.\]" } } */
/* { dg-final { scan-assembler "text,\"0x20000006\"" } } */
