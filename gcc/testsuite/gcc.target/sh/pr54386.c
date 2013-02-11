/* Check that the inlined mem load is not handled as unaligned load.  */
/* { dg-do compile { target "sh*-*-*" } } */
/* { dg-options "-O2" } */
/* { dg-skip-if "" { "sh*-*-*" } { "-m5*"} { "" } }  */
/* { dg-final { scan-assembler-not "shll|extu|or" } } */

static inline int
readint0 (int* x)
{
  return *x;
}

int
test0 (int* x)
{
  return readint0 (x);
}

inline int
readint1 (int* x)
{
  return *x;
}

int
test1 (int* x)
{
  return readint1 (x);
}

static int
readint2 (int* x)
{
  return *x;
}

int
test2 (int* x)
{
  return readint2 (x);
}
