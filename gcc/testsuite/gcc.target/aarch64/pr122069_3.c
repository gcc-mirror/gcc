/* { dg-do compile } */
/* { dg-options "-O3 -march=armv8.2-a+dotprod -mautovec-preference=asimd-only --param vect-epilogues-nomask=0 -fno-schedule-insns -fno-reorder-blocks -fno-schedule-insns2 -fdump-tree-vect-details" }*/
/* { dg-final { check-function-bodies "**" "" } } */

inline char char_abs(char i) {
  return (i < 0 ? -i : i);
}

/*
** foo_int:
** 	...
** 	sub	v[0-9]+.16b, v[0-9]+.16b, v[0-9]+.16b
** 	udot	v[0-9]+.4s, v[0-9]+.16b, v[0-9]+.16b
** 	...
*/
int foo_int(unsigned char *x, unsigned char * restrict y) {
  int sum = 0;
  for (int i = 0; i < 8000; i++)
     sum += char_abs(x[i] - y[i]);
  return sum;
}

/*
** foo2_int:
** 	...
** 	add	v[0-9]+.8h, v[0-9]+.8h, v[0-9]+.8h
** 	uaddw	v[0-9]+.4s, v[0-9]+.4s, v[0-9]+.4h
** 	uaddw2	v[0-9]+.4s, v[0-9]+.4s, v[0-9]+.8h
** 	...
*/
int foo2_int(unsigned short *x, unsigned short * restrict y) {
  int sum = 0;
  for (int i = 0; i < 8000; i++)
    {
      x[i] = x[i] + y[i];
      sum += x[i];
    }
  return sum;
}

/* { dg-final { scan-tree-dump-times "LOOP VECTORIZED" 2 "vect" } } */
