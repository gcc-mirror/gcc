/* { dg-do compile } */
/* { dg-options "-O2 -fPIC" } */

/* { dg-final { scan-assembler "@ltoffx\\(object#\\)" } } */
/* { dg-final { scan-assembler "@ltoffx\\(object#\[-+\]16384\\)" } } */
/* { dg-final { scan-assembler-not "@ltoffx\\(object#\[-+\]1\\)" } } */
/* { dg-final { scan-assembler-not "@ltoffx\\(object#\[-+\]8191\\)" } } */
/* { dg-final { scan-assembler-not "@ltoffx\\(object#\[-+\]8192\\)" } } */
/* { dg-final { scan-assembler-not "@ltoffx\\(object#\[-+\]8193\\)" } } */
/* { dg-final { scan-assembler-not "@ltoffx\\(object#\[-+\]16383\\)" } } */
/* { dg-final { scan-assembler-not "@ltoffx\\(object#\[-+\]16385\\)" } } */

/* must not be in sdata */
extern char object[];

#define r(n) char *r_##n (void) { return &object[n]; }
#define R(n) char *R_##n (void) { return &object[-n]; }

#define t(n) r(n) R(n)

t(0) t(1)
t(8191) t(8192) t(8193)
t(16383) t(16384) t(16385)
