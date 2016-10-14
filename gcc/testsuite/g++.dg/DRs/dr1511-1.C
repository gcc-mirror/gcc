/* DR 1511 - const volatile variables and the one-definition rule */
/* { dg-do run } */
/* { dg-additional-sources "dr1511-2.C" } */

typedef const int cint;
typedef const volatile int cvint;
typedef volatile int vint;
const int v1 = 5;
extern volatile const int v2;
cint v3 = 7;
extern cvint v4;
extern const vint v5;
extern volatile cint v6;
const int w1 = 5;
extern volatile const int w2;
cint w3 = 7;
extern cvint w4;
extern const vint w5;
extern volatile cint w6;
extern const int &r1;
extern volatile const int &r2;
extern const int &r3;
extern const volatile int &r4;
extern const volatile int &r5;
extern const volatile int &r6;

int
main ()
{
  if (v1 != 5 || v2 != 6 || v3 != 7 || v4 != 8 || v5 != 9 || v6 != 10)
    __builtin_abort ();
  if (w1 != 5 || w2 != 6 || w3 != 7 || w4 != 8 || w5 != 9 || w6 != 10)
    __builtin_abort ();
  if (r1 != w1 || &r1 == &w1 || r2 != w2 || &r2 != &w2 || r3 != w3 || &r3 == &w3)
    __builtin_abort ();
  if (r4 != w4 || &r4 != &w4 || r5 != w5 || &r5 != &w5 || r6 != w6 || &r6 != &w6)
    __builtin_abort ();
}
