/* { dg-do compile { target { powerpc*-*-linux* && lp64 } } } */
/* { dg-options "-mabi=elfv2" } */

struct f8
  {
    float x[8];
  };

void test (struct f8 a, struct f8 b) /* { dg-message "note: the ABI of passing homogeneous float aggregates will change" } */
{
}

