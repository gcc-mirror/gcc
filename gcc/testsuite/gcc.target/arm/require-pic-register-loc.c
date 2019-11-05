/* { dg-do compile } */
/* { dg-skip-if "-mpure-code and -fPIC incompatible" { *-*-* } { "-mpure-code" } } */
/* { dg-options "-g -fPIC" } */

void *v;
void a (void *x) { }
void b (void) { }
                       /* line 8.  */
int                    /* line 9.  */
main (int argc)        /* line 10.  */
{                      /* line 11.  */
  if (argc == 12345)   /* line 12.  */
    {
      a (v);
      return 1;
    }
  b ();

  return 0;
}

/* { dg-final { scan-assembler-not "\.loc 1 8 \[0-9\]\+" } } */
/* { dg-final { scan-assembler-not "\.loc 1 9 \[0-9\]\+" } } */
/* { dg-final { scan-assembler-not "\.loc 1 10 \[0-9\]\+" } } */

/* The loc at the start of the prologue.  */
/* { dg-final { scan-assembler-times "\.loc 1 11 \[0-9\]\+" 1 } } */

/* The loc at the end of the prologue, with the first user line.  */
/* { dg-final { scan-assembler-times "\.loc 1 12 \[0-9\]\+" 1 } } */
