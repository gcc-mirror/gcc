/* { dg-do run { target { riscv_v } } } */
/* { dg-additional-options "-O3 -minline-strcmp" } */

#include <string.h>

int
__attribute__ ((noipa, optimize ("0")))
foo2 (const char *s, const char *t, int n)
{
  return strncmp (s, t, n);
}

#define SZ 11

#define TEST(I, J, N)                                                          \
  int res_##I_##J_##N = __builtin_strncmp (s[I], s[J], N);                     \
  int ref_##I_##J_##N = foo2 (s[I], s[J], N);                                  \
  if (res_##I_##J_##N != ref_##I_##J_##N)                                      \
    __builtin_abort ();

int main ()
{
  const char *s[SZ]
    = {"",  "asdf", "0", "\0", "!@#$%***m1123fdnmoi43",
       "a", "z",    "1", "9",  "12345678901234567889012345678901234567890",
       "ds0fi0349r0sdmfvi0sjf0c9fj034mrx903cw0efmc9jfsicn2390crrm0i90msdfi0sdf0"};

  for (int i = 0; i < SZ; i++)
    for (int j = 0; j < SZ; j++)
      {
        TEST(i, j, 0)
        TEST(i, j, 1)
        TEST(i, j, 2)
        TEST(i, j, 3)
        TEST(i, j, 4)
        TEST(i, j, 5)
        TEST(i, j, 6)
        TEST(i, j, 7)
        TEST(i, j, 8)
        TEST(i, j, 9)
        TEST(i, j, 10)
        TEST(i, j, 11)
        TEST(i, j, 12)
        TEST(i, j, 13)
        TEST(i, j, 14)
        TEST(i, j, 15)
        TEST(i, j, 16)
        TEST(i, j, 17)
        TEST(i, j, 18)
        TEST(i, j, 19)
        TEST(i, j, 20)
        TEST(i, j, 21)
        TEST(i, j, 22)
        TEST(i, j, 23)
        TEST(i, j, 24)
        TEST(i, j, 25)
        TEST(i, j, 26)
        TEST(i, j, 27)
        TEST(i, j, 28)
        TEST(i, j, 29)
        TEST(i, j, 30)
        TEST(i, j, 31)
        TEST(i, j, 32)
        TEST(i, j, 33)
        TEST(i, j, 34)
        TEST(i, j, 35)
        TEST(i, j, 36)
        TEST(i, j, 37)
        TEST(i, j, 38)
        TEST(i, j, 39)
        TEST(i, j, 40)
        TEST(i, j, 41)
        TEST(i, j, 42)
        TEST(i, j, 43)
        TEST(i, j, 44)
        TEST(i, j, 45)
        TEST(i, j, 46)
        TEST(i, j, 47)
        TEST(i, j, 48)
        TEST(i, j, 49)
        TEST(i, j, 50)
        TEST(i, j, 51)
        TEST(i, j, 52)
        TEST(i, j, 53)
        TEST(i, j, 54)
        TEST(i, j, 55)
        TEST(i, j, 56)
        TEST(i, j, 57)
        TEST(i, j, 58)
        TEST(i, j, 59)
        TEST(i, j, 60)
        TEST(i, j, 61)
        TEST(i, j, 62)
        TEST(i, j, 63)
        TEST(i, j, 64)
        TEST(i, j, 65)
        TEST(i, j, 66)
        TEST(i, j, 67)
        TEST(i, j, 68)
        TEST(i, j, 69)
        TEST(i, j, 70)
        TEST(i, j, 71)
        TEST(i, j, 72)
        TEST(i, j, 73)
        TEST(i, j, 74)
        TEST(i, j, 75)
        TEST(i, j, 76)
        TEST(i, j, 77)
        TEST(i, j, 78)
        TEST(i, j, 79)
        TEST(i, j, 80)
        TEST(i, j, 81)
        TEST(i, j, 82)
        TEST(i, j, 83)
        TEST(i, j, 84)
        TEST(i, j, 85)
        TEST(i, j, 86)
        TEST(i, j, 87)
        TEST(i, j, 88)
        TEST(i, j, 89)
        TEST(i, j, 90)
        TEST(i, j, 91)
        TEST(i, j, 92)
        TEST(i, j, 93)
        TEST(i, j, 94)
        TEST(i, j, 95)
        TEST(i, j, 96)
        TEST(i, j, 97)
        TEST(i, j, 98)
        TEST(i, j, 99)
        TEST(i, j, 100)
        TEST(i, j, 101)
        TEST(i, j, 102)
        TEST(i, j, 103)
      }
}
