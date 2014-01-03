/* Test generation of floating-point compare custom instructions.  */

/* { dg-do compile } */
/* { dg-options "-O1" } */

/* -O1 in the options is significant.  Without it FP operations may not be
   optimized to custom instructions.  */

#pragma GCC target ("custom-frdxhi=40")
#pragma GCC target ("custom-frdxlo=41")
#pragma GCC target ("custom-frdy=42")
#pragma GCC target ("custom-fwrx=43")
#pragma GCC target ("custom-fwry=44")

#pragma GCC target ("custom-fcmpeqs=200")

int
test_fcmpeqs (float a, float b)
{
  return (a == b);
}

/* { dg-final { scan-assembler "custom\\t200, .* # fcmpeqs .*" } } */

#pragma GCC target ("custom-fcmpgtd=201")

int
test_fcmpgtd (double a, double b)
{
  return (a > b);
}

/* { dg-final { scan-assembler "custom\\t201, .* # fcmpgtd .*" } } */

#pragma GCC target ("custom-fcmples=202")

int
test_fcmples (float a, float b)
{
  return (a <= b);
}

/* { dg-final { scan-assembler "custom\\t202, .* # fcmples .*" } } */

#pragma GCC target ("custom-fcmpned=203")

int
test_fcmpned (double a, double b)
{
  return (a != b);
}

/* { dg-final { scan-assembler "custom\\t203, .* # fcmpned .*" } } */
