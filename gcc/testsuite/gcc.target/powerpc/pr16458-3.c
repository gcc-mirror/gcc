/* Test cse'ing of unsigned compares.  */
/* { dg-do compile } */
/* { dg-options "-O2 -fno-jump-tables" } */

/* { dg-final { scan-assembler-not "cmpwi" } } */
/* { dg-final { scan-assembler-times "cmplwi" 5 } } */

extern int case0 (void);
extern int case1 (void);
extern int case2 (void);
extern int case3 (void);
extern int case4 (void);

enum CASE_VALUES
{
  CASE0 = 1,
  CASE1,
  CASE2,
  CASE3,
  CASE4
};

int
foo (enum CASE_VALUES index)
{
  switch (index)
    {
    case CASE0:
      return case0 ();
    case CASE1:
      return case1 ();
    case CASE2:
      return case2 ();
    case CASE3:
      return case3 ();
    case CASE4:
      return case4 ();
    }

  return 0;
}
