/* { dg-do run } */
/* { dg-require-effective-target cilkplus_runtime } */
/* { dg-options "-fcilkplus" } */
/* { dg-additional-options "-std=gnu99" { target c } } */

int msk;

#define BODY \
  do {									\
    int j = (i >= 30U) ? 30 : i;					\
    if (__atomic_fetch_or (&msk, 1 << j, __ATOMIC_RELAXED) & (1 << j))	\
      __builtin_abort ();						\
  } while (0)
#define TEST(x) if (msk != (x)) __builtin_abort (); msk = 0

__attribute__((noinline, noclone)) void
test (int seven, int three, int two, int minustwo, int ten,
      int zero, int eleven, int six, int one, int threealt,
      unsigned long int sevenUL, unsigned long int threeUL,
      unsigned long int twoUL, unsigned long int minustwoUL,
      unsigned long int tenUL, unsigned long int zeroUL,
      unsigned long int elevenUL, unsigned long int sixUL,
      unsigned long int oneUL, unsigned long int threealtUL)
{
  _Cilk_for (int i = seven; i < three; ++i)
    __builtin_abort ();
  _Cilk_for (int i = seven; i <= three; ++i)
    __builtin_abort ();
  _Cilk_for (int i = three; i != threealt; ++i)
    __builtin_abort ();
  _Cilk_for (int i = seven; i < three; i += two)
    __builtin_abort ();
  _Cilk_for (int i = seven; i <= three; i += two)
    __builtin_abort ();
  _Cilk_for (int i = three; i != threealt; i += two)
    __builtin_abort ();
  _Cilk_for (int i = seven; i < three; i -= minustwo)
    __builtin_abort ();
  _Cilk_for (int i = seven; i <= three; i -= minustwo)
    __builtin_abort ();
  _Cilk_for (int i = three; i != threealt; i -= minustwo)
    __builtin_abort ();
  _Cilk_for (int i = three; i > seven; --i)
    __builtin_abort ();
  _Cilk_for (int i = three; i >= seven; i--)
    __builtin_abort ();
  _Cilk_for (int i = three; i != threealt; i--)
    __builtin_abort ();
  _Cilk_for (int i = three; i > seven; i -= two)
    __builtin_abort ();
  _Cilk_for (int i = three; i >= seven; i -= two)
    __builtin_abort ();
  _Cilk_for (int i = three; i != threealt; i -= two)
    __builtin_abort ();
  _Cilk_for (int i = three; i > seven; i += minustwo)
    __builtin_abort ();
  _Cilk_for (int i = three; i >= seven; i += minustwo)
    __builtin_abort ();
  _Cilk_for (int i = three; i != threealt; i += minustwo)
    __builtin_abort ();
  _Cilk_for (int i = three; i < seven; ++i)
    BODY;
  TEST (0x78);
  _Cilk_for (int i = three; i <= seven; i++)
    BODY;
  TEST (0xf8);
  _Cilk_for (int i = three; i != seven; i++)
    BODY;
  TEST (0x78);
  _Cilk_for (int i = zero; i < ten; i += two)
    BODY;
  TEST (0x155);
  _Cilk_for (int i = zero; i <= ten; i += two)
    BODY;
  TEST (0x555);
  _Cilk_for (int i = zero; i != ten; i += two)
    BODY;
  TEST (0x155);
  _Cilk_for (int i = zero; i < ten; i -= minustwo)
    BODY;
  TEST (0x155);
  _Cilk_for (int i = zero; i <= ten; i -= minustwo)
    BODY;
  TEST (0x555);
  _Cilk_for (int i = zero; i != ten; i -= minustwo)
    BODY;
  TEST (0x155);
  _Cilk_for (int i = six; i > two; --i)
    BODY;
  TEST (0x78);
  _Cilk_for (int i = seven; i >= three; i--)
    BODY;
  TEST (0xf8);
  _Cilk_for (int i = seven; i != three; i--)
    BODY;
  TEST (0xf0);
  _Cilk_for (int i = eleven; i > one; i += minustwo)
    BODY;
  TEST (0xaa8);
  _Cilk_for (int i = eleven; i >= two; i += minustwo)
    BODY;
  TEST (0xaa8);
  _Cilk_for (int i = eleven; i != one; i += minustwo)
    BODY;
  TEST (0xaa8);
  _Cilk_for (int i = eleven; i > one; i -= two)
    BODY;
  TEST (0xaa8);
  _Cilk_for (int i = eleven; i >= two; i -= two)
    BODY;
  TEST (0xaa8);
  _Cilk_for (int i = eleven; i != one; i -= two)
    BODY;
  TEST (0xaa8);
  _Cilk_for (unsigned long int i = sevenUL; i < threeUL; ++i)
    __builtin_abort ();
  _Cilk_for (unsigned long int i = sevenUL; i <= threeUL; ++i)
    __builtin_abort ();
  _Cilk_for (unsigned long int i = threeUL; i != threealtUL; ++i)
    __builtin_abort ();
  _Cilk_for (unsigned long int i = sevenUL; i < threeUL; i += twoUL)
    __builtin_abort ();
  _Cilk_for (unsigned long int i = sevenUL; i <= threeUL; i += twoUL)
    __builtin_abort ();
  _Cilk_for (unsigned long int i = threeUL; i != threealtUL; i += twoUL)
    __builtin_abort ();
  _Cilk_for (unsigned long int i = sevenUL; i < threeUL; i -= minustwoUL)
    __builtin_abort ();
  _Cilk_for (unsigned long int i = sevenUL; i <= threeUL; i -= minustwoUL)
    __builtin_abort ();
  _Cilk_for (unsigned long int i = threeUL; i != threealtUL; i -= minustwoUL)
    __builtin_abort ();
  _Cilk_for (unsigned long int i = threeUL; i > sevenUL; --i)
    __builtin_abort ();
  _Cilk_for (unsigned long int i = threeUL; i >= sevenUL; i--)
    __builtin_abort ();
  _Cilk_for (unsigned long int i = threeUL; i != threealtUL; i--)
    __builtin_abort ();
  _Cilk_for (unsigned long int i = threeUL; i > sevenUL; i -= twoUL)
    __builtin_abort ();
  _Cilk_for (unsigned long int i = threeUL; i >= sevenUL; i -= twoUL)
    __builtin_abort ();
  _Cilk_for (unsigned long int i = threeUL; i != threealtUL; i -= twoUL)
    __builtin_abort ();
  _Cilk_for (unsigned long int i = threeUL; i > sevenUL; i += minustwoUL)
    __builtin_abort ();
  _Cilk_for (unsigned long int i = threeUL; i >= sevenUL; i += minustwoUL)
    __builtin_abort ();
  _Cilk_for (unsigned long int i = threeUL; i != threealtUL; i += minustwoUL)
    __builtin_abort ();
  _Cilk_for (unsigned long int i = threeUL; i < sevenUL; ++i)
    BODY;
  TEST (0x78);
  _Cilk_for (unsigned long int i = threeUL; i <= sevenUL; i++)
    BODY;
  TEST (0xf8);
  _Cilk_for (unsigned long int i = threeUL; i != sevenUL; i++)
    BODY;
  TEST (0x78);
  _Cilk_for (unsigned long int i = zeroUL; i < tenUL; i += twoUL)
    BODY;
  TEST (0x155);
  _Cilk_for (unsigned long int i = zeroUL; i <= tenUL; i += twoUL)
    BODY;
  TEST (0x555);
  _Cilk_for (unsigned long int i = zeroUL; i != tenUL; i += twoUL)
    BODY;
  TEST (0x155);
  _Cilk_for (unsigned long int i = zeroUL; i < tenUL; i -= minustwoUL)
    BODY;
  TEST (0x155);
  _Cilk_for (unsigned long int i = zeroUL; i <= tenUL; i -= minustwoUL)
    BODY;
  TEST (0x555);
  _Cilk_for (unsigned long int i = zeroUL; i != tenUL; i -= minustwoUL)
    BODY;
  TEST (0x155);
  _Cilk_for (unsigned long int i = sixUL; i > twoUL; --i)
    BODY;
  TEST (0x78);
  _Cilk_for (unsigned long int i = sevenUL; i >= threeUL; i--)
    BODY;
  TEST (0xf8);
  _Cilk_for (unsigned long int i = sevenUL; i != threeUL; i--)
    BODY;
  TEST (0xf0);
  _Cilk_for (unsigned long int i = elevenUL; i > oneUL; i += minustwoUL)
    BODY;
  TEST (0xaa8);
  _Cilk_for (unsigned long int i = elevenUL; i >= twoUL; i += minustwoUL)
    BODY;
  TEST (0xaa8);
  _Cilk_for (unsigned long int i = elevenUL; i != oneUL; i += minustwoUL)
    BODY;
  TEST (0xaa8);
  _Cilk_for (unsigned long int i = elevenUL; i > oneUL; i -= twoUL)
    BODY;
  TEST (0xaa8);
  _Cilk_for (unsigned long int i = elevenUL; i >= twoUL; i -= twoUL)
    BODY;
  TEST (0xaa8);
  _Cilk_for (unsigned long int i = elevenUL; i != oneUL; i -= twoUL)
    BODY;
  TEST (0xaa8);
}

int
main ()
{
  _Cilk_for (int i = 7; i < 3; ++i)
    __builtin_abort ();
  _Cilk_for (int i = 7; i <= 3; ++i)
    __builtin_abort ();
  _Cilk_for (int i = 3; i != 3; ++i)
    __builtin_abort ();
  _Cilk_for (int i = 7; i < 3; i += 2)
    __builtin_abort ();
  _Cilk_for (int i = 7; i <= 3; i += 2)
    __builtin_abort ();
  _Cilk_for (int i = 3; i != 3; i += 2)
    __builtin_abort ();
  _Cilk_for (int i = 7; i < 3; i -= -2)
    __builtin_abort ();
  _Cilk_for (int i = 7; i <= 3; i -= -2)
    __builtin_abort ();
  _Cilk_for (int i = 3; i != 3; i -= -2)
    __builtin_abort ();
  _Cilk_for (int i = 3; i > 7; --i)
    __builtin_abort ();
  _Cilk_for (int i = 3; i >= 7; i--)
    __builtin_abort ();
  _Cilk_for (int i = 3; i != 3; i--)
    __builtin_abort ();
  _Cilk_for (int i = 3; i > 7; i -= 2)
    __builtin_abort ();
  _Cilk_for (int i = 3; i >= 7; i -= 2)
    __builtin_abort ();
  _Cilk_for (int i = 3; i != 3; i -= 2)
    __builtin_abort ();
  _Cilk_for (int i = 3; i > 7; i += -2)
    __builtin_abort ();
  _Cilk_for (int i = 3; i >= 7; i += -2)
    __builtin_abort ();
  _Cilk_for (int i = 3; i != 3; i += -2)
    __builtin_abort ();
  _Cilk_for (int i = 3; i < 7; ++i)
    BODY;
  TEST (0x78);
  _Cilk_for (int i = 3; i <= 7; i++)
    BODY;
  TEST (0xf8);
  _Cilk_for (int i = 3; i != 7; i++)
    BODY;
  TEST (0x78);
  _Cilk_for (int i = 0; i < 10; i += 2)
    BODY;
  TEST (0x155);
  _Cilk_for (int i = 0; i <= 10; i += 2)
    BODY;
  TEST (0x555);
  _Cilk_for (int i = 0; i != 10; i += 2)
    BODY;
  TEST (0x155);
  _Cilk_for (int i = 0; i < 10; i -= -2)
    BODY;
  TEST (0x155);
  _Cilk_for (int i = 0; i <= 10; i -= -2)
    BODY;
  TEST (0x555);
  _Cilk_for (int i = 0; i != 10; i -= -2)
    BODY;
  TEST (0x155);
  _Cilk_for (int i = 6; i > 2; --i)
    BODY;
  TEST (0x78);
  _Cilk_for (int i = 7; i >= 3; i--)
    BODY;
  TEST (0xf8);
  _Cilk_for (int i = 7; i != 3; i--)
    BODY;
  TEST (0xf0);
  _Cilk_for (int i = 11; i > 1; i += -2)
    BODY;
  TEST (0xaa8);
  _Cilk_for (int i = 11; i >= 2; i += -2)
    BODY;
  TEST (0xaa8);
  _Cilk_for (int i = 11; i != 1; i += -2)
    BODY;
  TEST (0xaa8);
  _Cilk_for (int i = 11; i > 1; i -= 2)
    BODY;
  TEST (0xaa8);
  _Cilk_for (int i = 11; i >= 2; i -= 2)
    BODY;
  TEST (0xaa8);
  _Cilk_for (int i = 11; i != 1; i -= 2)
    BODY;
  TEST (0xaa8);
  _Cilk_for (unsigned long int i = 7UL; i < 3UL; ++i)
    __builtin_abort ();
  _Cilk_for (unsigned long int i = 7UL; i <= 3UL; ++i)
    __builtin_abort ();
  _Cilk_for (unsigned long int i = 3UL; i != 3UL; ++i)
    __builtin_abort ();
  _Cilk_for (unsigned long int i = 7UL; i < 3UL; i += 2UL)
    __builtin_abort ();
  _Cilk_for (unsigned long int i = 7UL; i <= 3UL; i += 2UL)
    __builtin_abort ();
  _Cilk_for (unsigned long int i = 3UL; i != 3UL; i += 2UL)
    __builtin_abort ();
  _Cilk_for (unsigned long int i = 7UL; i < 3UL; i -= -2UL)
    __builtin_abort ();
  _Cilk_for (unsigned long int i = 7UL; i <= 3UL; i -= -2UL)
    __builtin_abort ();
  _Cilk_for (unsigned long int i = 3UL; i != 3UL; i -= -2UL)
    __builtin_abort ();
  _Cilk_for (unsigned long int i = 3UL; i > 7UL; --i)
    __builtin_abort ();
  _Cilk_for (unsigned long int i = 3UL; i >= 7UL; i--)
    __builtin_abort ();
  _Cilk_for (unsigned long int i = 3UL; i != 3UL; i--)
    __builtin_abort ();
  _Cilk_for (unsigned long int i = 3UL; i > 7UL; i -= 2UL)
    __builtin_abort ();
  _Cilk_for (unsigned long int i = 3UL; i >= 7UL; i -= 2UL)
    __builtin_abort ();
  _Cilk_for (unsigned long int i = 3UL; i != 3UL; i -= 2UL)
    __builtin_abort ();
  _Cilk_for (unsigned long int i = 3UL; i > 7UL; i += -2UL)
    __builtin_abort ();
  _Cilk_for (unsigned long int i = 3UL; i >= 7UL; i += -2UL)
    __builtin_abort ();
  _Cilk_for (unsigned long int i = 3UL; i != 3UL; i += -2UL)
    __builtin_abort ();
  _Cilk_for (unsigned long int i = 3UL; i < 7UL; ++i)
    BODY;
  TEST (0x78);
  _Cilk_for (unsigned long int i = 3UL; i <= 7UL; i++)
    BODY;
  TEST (0xf8);
  _Cilk_for (unsigned long int i = 3UL; i != 7UL; i++)
    BODY;
  TEST (0x78);
  _Cilk_for (unsigned long int i = 0UL; i < 10UL; i += 2UL)
    BODY;
  TEST (0x155);
  _Cilk_for (unsigned long int i = 0UL; i <= 10UL; i += 2UL)
    BODY;
  TEST (0x555);
  _Cilk_for (unsigned long int i = 0UL; i != 10UL; i += 2UL)
    BODY;
  TEST (0x155);
  _Cilk_for (unsigned long int i = 0UL; i < 10UL; i -= -2UL)
    BODY;
  TEST (0x155);
  _Cilk_for (unsigned long int i = 0UL; i <= 10UL; i -= -2UL)
    BODY;
  TEST (0x555);
  _Cilk_for (unsigned long int i = 0UL; i != 10UL; i -= -2UL)
    BODY;
  TEST (0x155);
  _Cilk_for (unsigned long int i = 6UL; i > 2UL; --i)
    BODY;
  TEST (0x78);
  _Cilk_for (unsigned long int i = 7UL; i >= 3UL; i--)
    BODY;
  TEST (0xf8);
  _Cilk_for (unsigned long int i = 7UL; i != 3UL; i--)
    BODY;
  TEST (0xf0);
  _Cilk_for (unsigned long int i = 11UL; i > 1UL; i += -2UL)
    BODY;
  TEST (0xaa8);
  _Cilk_for (unsigned long int i = 11UL; i >= 2UL; i += -2UL)
    BODY;
  TEST (0xaa8);
  _Cilk_for (unsigned long int i = 11UL; i != 1UL; i += -2UL)
    BODY;
  TEST (0xaa8);
  _Cilk_for (unsigned long int i = 11UL; i > 1UL; i -= 2UL)
    BODY;
  TEST (0xaa8);
  _Cilk_for (unsigned long int i = 11UL; i >= 2UL; i -= 2UL)
    BODY;
  TEST (0xaa8);
  _Cilk_for (unsigned long int i = 11UL; i != 1UL; i -= 2UL)
    BODY;
  TEST (0xaa8);
  test (7, 3, 2, -2, 10, 0, 11, 6, 1, 3,
	7UL, 3UL, 2UL, -2UL, 10UL, 0UL, 11UL, 6UL, 1UL, 3UL);
  return 0;
}
