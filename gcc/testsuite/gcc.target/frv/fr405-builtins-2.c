/* Test the SCUTSS instruction.  */
/* { dg-options "-mcpu=fr405" } */
/* { dg-do run } */
extern void abort (void);
extern void exit (int);

int main ()
{
  struct {
    long long value;
    int cut_point;
    int result;
  } values[] = {
    /* Non-saturating values */

    { +0x0000000000001234LL, 44, +0x01234000 },
    { ~0x0000000000001234LL, 44, ~0x01234fff },

    { +0x0000011223300fffLL, 20, +0x11223301 },
    { ~0x0000011223300fffLL, 20, ~0x11223300 },
    { +0x0000011223300800LL, 20, +0x11223301 },
    { ~0x0000011223300800LL, 20, ~0x11223300 },
    { +0x00000112233007ffLL, 20, +0x11223300 },
    { ~0x00000112233007ffLL, 20, ~0x112232ff },
    { +0x0000011223300000LL, 20, +0x11223300 },
    { ~0x0000011223300000LL, 20, ~0x112232ff },

    { +0x1234567fffffffffLL, -4, +0x01234568 },
    { ~0x1234567fffffffffLL, -4, ~0x01234567 },
    { +0x1234567800000000LL, -4, +0x01234568 },
    { ~0x1234567800000000LL, -4, ~0x01234567 },
    { +0x12345677ffffffffLL, -4, +0x01234567 },
    { ~0x12345677ffffffffLL, -4, ~0x01234566 },
    { +0x1234567000000000LL, -4, +0x01234567 },
    { ~0x1234567000000000LL, -4, ~0x01234566 },

    /* Saturation tests */

    { +0x4000000000000000LL, 44, +0x7fffffff },
    { ~0x4000000000000000LL, 44, ~0x7fffffff },
    { +0x0000000000080000LL, 44, +0x7fffffff },
    { ~0x0000000000080000LL, 44, ~0x7fffffff },
    { +0x000000000007ffffLL, 44, +0x7ffff000 },
    { ~0x000000000007ffffLL, 44, ~0x7fffffff },
    { +0x000000000007fffeLL, 44, +0x7fffe000 },
    { ~0x000000000007fffeLL, 44, ~0x7fffefff },

    { +0x4000000000000000LL, 20, +0x7fffffff },
    { ~0x4000000000000000LL, 20, ~0x7fffffff },
    { +0x0000080000000000LL, 20, +0x7fffffff },
    { ~0x0000080000000000LL, 20, ~0x7fffffff },
    { +0x000007ffffffffffLL, 20, +0x7fffffff },
    { ~0x000007ffffffffffLL, 20, ~0x7fffffff },
    { +0x000007fffffff000LL, 20, +0x7fffffff },
    { ~0x000007fffffff000LL, 20, ~0x7ffffffe },
    { +0x000007ffffffe000LL, 20, +0x7ffffffe },
    { ~0x000007ffffffefffLL, 20, ~0x7ffffffe }
  };

  unsigned int i;

  for (i = 0; i < sizeof (values) / sizeof (values[0]); i++)
    {
      __IACCsetll (0, values[i].value);
      if (__SCUTSS (values[i].cut_point) != values[i].result)
	abort ();
    }
  exit (0);
}
