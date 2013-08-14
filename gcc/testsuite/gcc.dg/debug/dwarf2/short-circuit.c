/* { dg-do compile } */
/* { dg-options "-O0 -gdwarf -dA" } */

#define True  1
#define False 0

_Bool F1 (_Bool B1, _Bool B2)
{
  _Bool R;

  if (B1
      && B2)
    R = True;
  else
    R = False;
  return R;
}

_Bool F2 (_Bool B1, _Bool B2)
{
  _Bool R;

  R = B1
      && B2;
  return R;
}

_Bool F3 (_Bool B1, _Bool B2)
{
  _Bool R = False;

  if (B1
      && B2)
    R = True;
  return R;
}

_Bool F4 (_Bool B1, _Bool B2)
{
  _Bool R = False;

  if (B1
      || B2)
    ;
  else
    R = True;
  return R;
}

_Bool F5 (_Bool B1, _Bool B2)
{
  _Bool R = False;

  if (!(B1
        && B2))
    R = True;
  return R;
}

_Bool F8 (_Bool B1, _Bool B2, _Bool B3, _Bool B4, _Bool B5, _Bool B6,
          _Bool B7, _Bool B8)
{
  _Bool R;

  if ((B1
       || B2)
       && B3
       && !(B4
            || B5)
       && (B6
           || (B7
               && B8)))
    R = True;
  else
    R = False;
  return R;
}

/* { dg-final { scan-assembler "short-circuit.c:11" } } */
/* { dg-final { scan-assembler "short-circuit.c:12" } } */
/* { dg-final { scan-assembler "short-circuit.c:13" } } */
/* { dg-final { scan-assembler "short-circuit.c:15" } } */

/* { dg-final { scan-assembler "short-circuit.c:23" } } */
/* { dg-final { scan-assembler "short-circuit.c:24" } } */

/* { dg-final { scan-assembler "short-circuit.c:32" } } */
/* { dg-final { scan-assembler "short-circuit.c:33" } } */
/* { dg-final { scan-assembler "short-circuit.c:34" } } */

/* { dg-final { scan-assembler "short-circuit.c:42" } } */
/* { dg-final { scan-assembler "short-circuit.c:43" } } */
/* { dg-final { scan-assembler "short-circuit.c:46" } } */

/* { dg-final { scan-assembler "short-circuit.c:54" } } */
/* { dg-final { scan-assembler "short-circuit.c:55" } } */
/* { dg-final { scan-assembler "short-circuit.c:56" } } */

/* { dg-final { scan-assembler "short-circuit.c:65" } } */
/* { dg-final { scan-assembler "short-circuit.c:66" } } */
/* { dg-final { scan-assembler "short-circuit.c:67" } } */
/* { dg-final { scan-assembler "short-circuit.c:68" } } */
/* { dg-final { scan-assembler "short-circuit.c:69" } } */
/* { dg-final { scan-assembler "short-circuit.c:70" } } */
/* { dg-final { scan-assembler "short-circuit.c:71" } } */
/* { dg-final { scan-assembler "short-circuit.c:72" } } */
/* { dg-final { scan-assembler "short-circuit.c:73" } } */
/* { dg-final { scan-assembler "short-circuit.c:75" } } */
