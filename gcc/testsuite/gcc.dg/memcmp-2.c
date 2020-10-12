/* PR middle-end/78257 - missing memcmp optimization with constant arrays
   { dg-do compile }
   { dg-options "-O -Wall -fdump-tree-optimized" } */

#define assert(e) ((e) ? (void)0 : __builtin_abort ())

typedef __INT32_TYPE__ int32_t;

extern int memcmp (const void*, const void*, __SIZE_TYPE__);

const int32_t i_0 = 0;
const int32_t j_0 = 0;

void eq_i0_j0 (void)
{
  const char *pi = (char*)&i_0, *pj = (char*)&j_0;
  int n = 0;

  n += 0 == memcmp (pi,     pj,     sizeof (int32_t));
  n += 0 == memcmp (pi + 1, pj + 1, sizeof (int32_t) - 1);
  n += 0 == memcmp (pi + 2, pj + 2, sizeof (int32_t) - 2);
  n += 0 == memcmp (pi + 3, pj + 3, sizeof (int32_t) - 3);
  n += 0 == memcmp (pi + 4, pj + 4, sizeof (int32_t) - 4);

  assert (n == 5);
}


const int32_t i1234 = 1234;
const int32_t j1234 = 1234;

void eq_i1234_j1245 (void)
{
  const char *pi = (char*)&i1234, *pj = (char*)&j1234;
  int n = 0;

  n += 0 == memcmp (pi,     pj,     sizeof (int32_t));
  n += 0 == memcmp (pi + 1, pj + 1, sizeof (int32_t) - 1);
  n += 0 == memcmp (pi + 2, pj + 2, sizeof (int32_t) - 2);
  n += 0 == memcmp (pi + 3, pj + 3, sizeof (int32_t) - 3);
  n += 0 == memcmp (pi + 4, pj + 4, sizeof (int32_t) - 4);

  assert (n == 5);
}


const int32_t a1[2] = { 1234 };
const int32_t b1[2] = { 1234 };

void eq_a1_b1 (void)
{
  const char *pi = (char*)&a1, *pj = (char*)&b1;
  int n = 0, nb = sizeof a1;

  n += 0 == memcmp (pi,     pj,     nb);
  n += 0 == memcmp (pi + 1, pj + 1, nb - 1);
  n += 0 == memcmp (pi + 2, pj + 2, nb - 2);
  n += 0 == memcmp (pi + 3, pj + 3, nb - 3);
  n += 0 == memcmp (pi + 4, pj + 4, nb - 4);
  n += 0 == memcmp (pi + 5, pj + 5, nb - 5);
  n += 0 == memcmp (pi + 6, pj + 6, nb - 6);
  n += 0 == memcmp (pi + 7, pj + 7, nb - 7);
  n += 0 == memcmp (pi + 8, pj + 8, nb - 8);

  assert (n == 9);
}

const int32_t a2[2] = { 1234 };
const int32_t b2[2] = { 1234, 0 };

void eq_a2_b2 (void)
{
  const char *pi = (char*)&a2, *pj = (char*)&b2;
  int n = 0, nb = sizeof a2;

  n += 0 == memcmp (pi,     pj,     nb);
  n += 0 == memcmp (pi + 1, pj + 1, nb - 1);
  n += 0 == memcmp (pi + 2, pj + 2, nb - 2);
  n += 0 == memcmp (pi + 3, pj + 3, nb - 3);
  n += 0 == memcmp (pi + 4, pj + 4, nb - 4);
  n += 0 == memcmp (pi + 5, pj + 5, nb - 5);
  n += 0 == memcmp (pi + 6, pj + 6, nb - 6);
  n += 0 == memcmp (pi + 7, pj + 7, nb - 7);
  n += 0 == memcmp (pi + 8, pj + 8, nb - 8);

  assert (n == 9);
}


const int32_t a5[5] = { [3] = 1234, [1] = 0 };
const int32_t b5[5] = { 0, 0, 0, 1234 };

void eq_a5_b5 (void)
{
  int n = 0, b = sizeof a5;
  const char *pi = (char*)a5, *pj = (char*)b5;

  n += 0 == memcmp (pi, pj, b);
  n += 0 == memcmp (pi + 1, pj + 1, b - 1);
  n += 0 == memcmp (pi + 2, pj + 2, b - 2);
  n += 0 == memcmp (pi + 3, pj + 3, b - 3);

  n += 0 == memcmp (pi + 4, pj + 4, b - 4);
  n += 0 == memcmp (pi + 5, pj + 5, b - 5);
  n += 0 == memcmp (pi + 6, pj + 6, b - 6);
  n += 0 == memcmp (pi + 7, pj + 7, b - 7);

  n += 0 == memcmp (pi + 8, pj + 8, b - 8);
  n += 0 == memcmp (pi + 9, pj + 9, b - 9);
  n += 0 == memcmp (pi + 10, pj + 10, b - 10);
  n += 0 == memcmp (pi + 11, pj + 11, b - 11);

  n += 0 == memcmp (pi + 12, pj + 12, b - 12);
  n += 0 == memcmp (pi + 13, pj + 13, b - 13);
  n += 0 == memcmp (pi + 14, pj + 14, b - 14);
  n += 0 == memcmp (pi + 15, pj + 15, b - 15);

  n += 0 == memcmp (pi + 16, pj + 16, b - 16);
  n += 0 == memcmp (pi + 17, pj + 17, b - 17);
  n += 0 == memcmp (pi + 18, pj + 18, b - 18);
  n += 0 == memcmp (pi + 19, pj + 19, b - 19);

  assert (n == 20);
}


const int32_t a19[19] = { [13] = 13, [8] = 8, [4] = 4, [1] = 1  };
const int32_t b19[19] = { 0, 1, 0, 0, 4, 0, 0, 0, 8, 0, 0, 0, 0, 13 };

void eq_a19_b19 (void)
{
  int n = 0, b = sizeof a19;
  const char *pi = (char*)a19, *pj = (char*)b19;

  n += 0 == memcmp (pi,     pj,     b);
  n += 0 == memcmp (pi + 1, pj + 1, b - 1);
  n += 0 == memcmp (pi + 2, pj + 2, b - 2);
  n += 0 == memcmp (pi + 3, pj + 3, b - 3);

  n += 0 == memcmp (pi + 14, pj + 14, b - 14);
  n += 0 == memcmp (pi + 15, pj + 15, b - 15);
  n += 0 == memcmp (pi + 16, pj + 16, b - 16);
  n += 0 == memcmp (pi + 17, pj + 17, b - 17);

  n += 0 == memcmp (pi + 28, pj + 28, b - 28);
  n += 0 == memcmp (pi + 29, pj + 29, b - 29);
  n += 0 == memcmp (pi + 30, pj + 30, b - 30);
  n += 0 == memcmp (pi + 31, pj + 31, b - 31);

  n += 0 == memcmp (pi + 42, pj + 42, b - 42);
  n += 0 == memcmp (pi + 43, pj + 43, b - 43);
  n += 0 == memcmp (pi + 44, pj + 44, b - 44);
  n += 0 == memcmp (pi + 45, pj + 45, b - 45);

  n += 0 == memcmp (pi + 56, pj + 56, b - 56);
  n += 0 == memcmp (pi + 57, pj + 57, b - 57);
  n += 0 == memcmp (pi + 58, pj + 58, b - 58);
  n += 0 == memcmp (pi + 59, pj + 59, b - 59);

  assert (n == 20);
}


const int32_t A20[20] = { [13] = 14, [8] = 8, [4] = 4, [1] = 1  };
const int32_t b20[20] = { 0, 1, 0, 0, 4, 0, 0, 0, 8, 0, 0, 0, 0, 13 };

void gt_A20_b20 (void)
{
  int n = memcmp (A20, b20, sizeof A20) > 0;
  assert (n == 1);
}

const int32_t a21[21] = { [13] = 12, [8] = 8, [4] = 4, [1] = 1  };
const int32_t B21[21] = { 0, 1, 0, 0, 4, 0, 0, 0, 8, 0, 0, 0, 0, 13 };

void lt_a21_B21 (void)
{
  int n = memcmp (a21, B21, sizeof a21) < 0;
  assert (n == 1);
}


/* { dg-final { scan-tree-dump-not "abort" "optimized" } } */
