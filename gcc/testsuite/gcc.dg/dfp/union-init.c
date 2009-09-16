/* Cast to union is a GNU C extension.  */

#include "dfp-dbg.h"

union u
{
  _Decimal128 d128;
  double d;
};

union n
{
  double d;
  _Decimal64 d64;
};

int main ()
{
  static union u u1 = { 0.0dl };
  static union u u2 = { 4.2dl };
  static union u u4 = { 0.0 };

  static union n n1 = { 2.2dd };
  static union n n2 = { 3.25 };

  _Decimal128 d128;
  _Decimal64 d64;
  double d;
  
  if (u1.d128 != 0.0dl)
    FAILURE

  if (u2.d128 != 4.2dl)
    FAILURE

  /* cast decimal floating point to union type.  */
  d128 = 1.23dl;
  d64 = 4.56dd;
  d = 3.25;

  u4 = (union u) d128;
  if (u4.d128 != 1.23dl)
    FAILURE
  
  u4 = (union u) d;
  if (u4.d != 3.25)
    FAILURE

  n1 = (union n) d64;
  if (n1.d64 != 4.56dd)
    FAILURE
  
  n1 = (union n)d;
  if (n1.d != 3.25)
    FAILURE

  FINISH
}
