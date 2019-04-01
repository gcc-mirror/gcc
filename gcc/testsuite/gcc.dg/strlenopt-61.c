/* PR tree-optimization/89688 - -Wstringop-overflow confused by const
   2D array of char
   { dg-do compile }
   { dg-options "-Wall -fdump-tree-gimple -fdump-tree-optimized" } */

typedef __SIZE_TYPE__ size_t;

size_t strlen (const char*);
#define CAT(x, y) x ## y
#define CONCAT(x, y) CAT (x, y)
#define FAILNAME(name) CONCAT (call_ ## name ##_on_line_, __LINE__)

#define FAIL(name) do {                         \
    extern __attribute__ ((noreturn)) void FAILNAME (name) (void);	\
    FAILNAME (name)();                          \
  } while (0)

#define A(ref, len)					\
  if (strlen (ref) != len) FAIL (failure); else (void)0

const char a3_4[3][4] = { { 1 }, { 1, 2 }, { 1, 2, 3 } };

void test_a4_4 (void)
{
  A (a3_4[0], 1);
  A (a3_4[1], 2);
  A (a3_4[2], 3);

  A (&a3_4[0][0], 1);
  A (&a3_4[0][1], 0);
  A (&a3_4[0][2], 0);
  A (&a3_4[0][3], 0);

  A (&a3_4[1][0], 2);
  A (&a3_4[1][1], 1);
  A (&a3_4[1][2], 0);
  A (&a3_4[1][3], 0);

  A (&a3_4[2][0], 3);
  A (&a3_4[2][1], 2);
  A (&a3_4[2][2], 1);
  A (&a3_4[2][3], 0);
}


const char a3_4_5[3][4][5] =
  {
   { { 1 }, { 1, 2 }, { 1, 2, 3 }, { 1, 2, 3, 4 } },
   { { 1, 2 }, { 1, 2, 3 }, { 1, 2, 3, 4 }, { 1 } },
   { { 1, 2, 3 }, { 1, 2, 3, 4 }, { 1 }, { 1, 2 } },
  };

void test_a3_4_5 (void)
{
  A (a3_4_5[0][0], 1);
  A (a3_4_5[0][1], 2);
  A (a3_4_5[0][2], 3);
  A (a3_4_5[0][3], 4);

  A (a3_4_5[1][0], 2);
  A (a3_4_5[1][1], 3);
  A (a3_4_5[1][2], 4);
  A (a3_4_5[1][3], 1);

  A (a3_4_5[2][0], 3);
  A (a3_4_5[2][1], 4);
  A (a3_4_5[2][2], 1);
  A (a3_4_5[2][3], 2);
}


struct S
{
  char a3[3];
  char a4_5[4][5];
};

const struct S sa4[4] =
  {
   { .a3 = { 0 },
     .a4_5 =
     {
      { 1 }, { 1, 2 }, { 1, 2, 3 }, { 1, 2, 3, 4 }
     }
   },
   { .a3 = { 1 },
     .a4_5 =
     {
      { 1, 2 }, { 1, 2, 3 }, { 1, 2, 3, 4 }, { 1 }
     }
   },
   { .a3 = { 1, 2 },
     .a4_5 =
     {
      { 1, 2, 3 }, { 1, 2, 3, 4 }, { 1 }, { 1, 2 }
     }
   },
   { .a3 = { 1 },
     .a4_5 =
     {
      { 1, 2, 3, 4 }, "1", { 1, 2 }, "123"
     }
   }
  };

void test_sa4 (void)
{
  A (sa4[0].a3, 0);
  A (sa4[0].a4_5[0], 1);
  A (sa4[0].a4_5[1], 2);
  A (sa4[0].a4_5[2], 3);
  A (sa4[0].a4_5[3], 4);

  A (sa4[1].a3, 1);
  A (sa4[1].a4_5[0], 2);
  A (sa4[1].a4_5[1], 3);
  A (sa4[1].a4_5[2], 4);
  A (sa4[1].a4_5[3], 1);

  A (sa4[2].a3, 2);
  A (sa4[2].a4_5[0], 3);
  A (sa4[2].a4_5[1], 4);
  A (sa4[2].a4_5[2], 1);
  A (sa4[2].a4_5[3], 2);

  A (sa4[3].a3, 1);
  A (sa4[3].a4_5[0], 4);
  A (sa4[3].a4_5[1], 1);
  A (sa4[3].a4_5[2], 2);
  A (sa4[3].a4_5[3], 3);
}


struct T
{
  struct S sa2[2];
  char a4[4];
};

const struct T ta2[2] =
  {
   [0] =
   {
    .sa2 =
    {
     [0] =
     { .a3 = { 0 },
       .a4_5 =
       {
	{ 1 }, { 1, 2 }, { 1, 2, 3 }, { 1, 2, 3, 4 }
       }
     },
     [1] =
     { .a3 = { 1 },
       .a4_5 =
       {
	{ 1, 2 }, { 1, 2, 3 }, { 1, 2, 3, 4 }, { 1 }
       }
     },
    },
    .a4 = "12"
   },

   [1] =
   {
    .sa2 =
    {
     [0] =
     { .a3 = { 1, 2 },
       .a4_5 =
       {
	{ 1, 2, 3 }, { 1, 2, 3, 4 }, { 1 }, { 1, 2 }
       }
     },
     { .a3 = { 1 },
       .a4_5 =
       {
	{ 1, 2, 3, 4 }, "1", { 1, 2 }, "123"
       }
     }
    },
    .a4 = "123"
   }
  };

void test_ta2 (void)
{
  A (ta2[0].sa2[0].a3, 0);
  A (ta2[0].sa2[0].a4_5[0], 1);
  A (ta2[0].sa2[0].a4_5[1], 2);
  A (ta2[0].sa2[0].a4_5[2], 3);
  A (ta2[0].sa2[0].a4_5[3], 4);

  A (ta2[0].sa2[1].a3, 1);
  A (ta2[0].sa2[1].a4_5[0], 2);
  A (ta2[0].sa2[1].a4_5[1], 3);
  A (ta2[0].sa2[1].a4_5[2], 4);
  A (ta2[0].sa2[1].a4_5[3], 1);

  A (ta2[0].a4, 2);

  A (ta2[1].sa2[0].a3, 2);
  A (ta2[1].sa2[0].a4_5[0], 3);
  A (ta2[1].sa2[0].a4_5[1], 4);
  A (ta2[1].sa2[0].a4_5[2], 1);
  A (ta2[1].sa2[0].a4_5[3], 2);

  A (ta2[1].sa2[1].a3, 1);
  A (ta2[1].sa2[1].a4_5[0], 4);
  A (ta2[1].sa2[1].a4_5[1], 1);
  A (ta2[1].sa2[1].a4_5[2], 2);
  A (ta2[1].sa2[1].a4_5[3], 3);

  A (ta2[1].a4, 3);
}

/* { dg-final { scan-tree-dump-not "failure" "optimized" } }
   { dg-final { scan-tree-dump-not "strlen" "gimple" } } */
