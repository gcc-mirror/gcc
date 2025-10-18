/* { dg-do compile } */
/* { dg-options "-O3 -march=armv8-a+sve -mautovec-preference=sve-only --param vect-epilogues-nomask=0 -fno-schedule-insns -fno-reorder-blocks -fno-schedule-insns2 -fdump-tree-vect-details" }*/
/* { dg-final { check-function-bodies "**" "" } } */

inline char char_abs(char i) {
  return (i < 0 ? -i : i);
}

/*
** foo_int:
** 	...
** 	sub	z[0-9]+.b, z[0-9]+.b, z[0-9]+.b
** 	udot	z[0-9]+.s, z[0-9]+.b, z[0-9]+.b
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
** 	add	z[0-9]+.h, z[0-9]+.h, z[0-9]+.h
** 	punpklo	p[0-9]+.h, p[0-9]+.b
** 	uunpklo	z[0-9]+.s, z[0-9]+.h
** 	add	z[0-9]+.s, p[0-9]+/m, z[0-9]+.s, z[0-9]+.s
** 	punpkhi	p[0-9]+.h, p[0-9]+.b
** 	uunpkhi	z[0-9]+.s, z[0-9]+.h
** 	add	z[0-9]+.s, p[0-9]+/m, z[0-9]+.s, z[0-9]+.s
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
