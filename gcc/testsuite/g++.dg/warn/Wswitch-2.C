/* Further -Wswitch tests.  */
/* { dg-do compile } */
/* { dg-options "-Wswitch" } */

enum e { e1 = 0, e2 = 1, e3 = 1, e4 = 2 };

int
foo (enum e ei, int j)
{
  switch (ei)
    {
    case e1: return 1;
    case e3: return 2;
    case e4: return 3;
    }	/* No warning here since e2 has the same value as e3.  */
  switch (ei) /* { dg-warning "enumeration value 'e4' not handled in switch" "enum e4" } */
    {
    case e1: return 1;
    case e2: return 2;
    }
  switch ((int) ei)
    {
    case e1: return 1;
    }	/* No warning here since switch condition was cast to int.  */
  switch ((enum e) j) /* { dg-warning "enumeration value 'e1' not handled in switch" "enum e1" } */
    {
    case e2: return 1;
    case e4: return 2;
    }
  return 0;
}
