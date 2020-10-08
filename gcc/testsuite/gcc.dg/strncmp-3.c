/* PR middle-end/95189 - memcmp being wrongly stripped like strcmp
   { dg-do run }
   { dg-options "-O2 -Wall" } */

#define AB_D "ab\0d"
#define ABCDEF_H "abcdef\0h"
#define ABCDEFGHIJKLMN_P "abcdefghijklmn\0p"

char ab_d[] = AB_D;
char abcdef_h[] = ABCDEF_H;

extern int strncmp (const char*, const char*, __SIZE_TYPE__);

__attribute__((noipa)) void sink (const void *p, ...) { (void)&p; }

#define strncmp(a, b, n) (sink (a, b), strncmp (a, b, n))

int main (void)
{
  int zero = 0;

  zero += strncmp (ab_d, AB_D, 1);
  zero += strncmp (ab_d, AB_D, 2);
  zero += strncmp (ab_d, AB_D, 3);
  zero += strncmp (ab_d, AB_D, 4);
  zero += strncmp (ab_d, AB_D, 5);

  zero += strncmp (ab_d, ABCDEF_H, 1);
  zero += strncmp (ab_d, ABCDEF_H, 2);

  zero += strncmp (abcdef_h, AB_D, 2);

  zero += strncmp (abcdef_h, ABCDEF_H, 2);
  zero += strncmp (abcdef_h, ABCDEF_H, 3);
  zero += strncmp (abcdef_h, ABCDEF_H, 4);
  zero += strncmp (abcdef_h, ABCDEF_H, 5);
  zero += strncmp (abcdef_h, ABCDEF_H, 6);
  zero += strncmp (abcdef_h, ABCDEF_H, 7);
  zero += strncmp (abcdef_h, ABCDEF_H, 8);
  zero += strncmp (abcdef_h, ABCDEF_H, 9);

  if (zero != 0)
    __builtin_abort ();

  int neg = 0;

  neg -= strncmp (ab_d, ABCDEF_H, 3) < 0;
  neg -= strncmp (ab_d, ABCDEF_H, 4) < 0;
  neg -= strncmp (ab_d, ABCDEF_H, 5) < 0;
  neg -= strncmp (ab_d, ABCDEF_H, 6) < 0;
  neg -= strncmp (ab_d, ABCDEF_H, 7) < 0;
  neg -= strncmp (ab_d, ABCDEF_H, 8) < 0;
  neg -= strncmp (ab_d, ABCDEF_H, 9) < 0;

  if (neg != -7)
    __builtin_abort ();
}
