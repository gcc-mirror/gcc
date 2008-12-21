/* { dg-options "-O2 -mabi=64 -mr10k-cache-barrier=store" } */

/* Test that stores to constant cached addresses are protected
   by cache barriers.  */

#define TEST(ADDR)					\
  NOMIPS16 void						\
  test_##ADDR (int n)					\
  {							\
    *(volatile int *) (0x##ADDR##UL) = 1;		\
  }

TEST (8ffffffffffffffc)
TEST (9000010000000000)

TEST (91fffffffffffffc)
TEST (9200010000000000)

TEST (93fffffffffffffc)
TEST (9500010000000000)

TEST (95fffffffffffffc)
TEST (9600010000000000)

TEST (b7fffffffffffffc)
TEST (b800010000000000)

TEST (b9fffffffffffffc)
TEST (ba00010000000000)

TEST (bbfffffffffffffc)
TEST (bc00010000000000)

TEST (bdfffffffffffffc)
TEST (be00010000000000)

TEST (ffffffff9ffffffc)
TEST (ffffffffc0000000)

/* { dg-final { scan-assembler-times "\tcache\t" 18 } } */
