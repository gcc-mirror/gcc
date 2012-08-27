/* { dg-options "-mabi=64 -mr10k-cache-barrier=store" } */

/* Test that stores to uncached addresses do not get unnecessary
   cache barriers.  */

#define TEST(ADDR)					\
  NOMIPS16 void						\
  test_##ADDR (int n)					\
  {							\
    while (n--)						\
      {							\
	*(volatile char *) (0x##ADDR##UL) = 1;		\
	*(volatile short *) (0x##ADDR##UL + 2) = 2;	\
	*(volatile int *) (0x##ADDR##UL + 4) = 0;	\
      }							\
  }

TEST (9000000000000000)
TEST (900000fffffffff8)

TEST (9200000000000000)
TEST (920000fffffffff8)

TEST (9400000000000000)
TEST (940000fffffffff8)

TEST (9600000000000000)
TEST (960000fffffffff8)

TEST (b800000000000000)
TEST (b80000fffffffff8)

TEST (ba00000000000000)
TEST (ba0000fffffffff8)

TEST (bc00000000000000)
TEST (bc0000fffffffff8)

TEST (be00000000000000)
TEST (be0000fffffffff8)

TEST (ffffffffa0000000)
TEST (ffffffffbffffff8)

/* { dg-final { scan-assembler-not "\tcache\t" } } */
