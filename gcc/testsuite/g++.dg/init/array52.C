// PR c++/89980 - pointer initialization with empty string folded to zero
// { dg-do compile }
// { dg-options "-O2 -Wall -fdump-tree-optimized" }

#if __cplusplus >= 201103L

#define SA(e) static_assert (e, #e)

static constexpr const char* const ca1[2] = { "" };

void fca1 (void)
{
  SA (ca1[0] && ca1[0][0] == 0 && ca1[1] == 0);
}

static constexpr const char* const ca2[][2] =
{
  { }, { 0 }, { 0, 0 }, { "" }, { "", "" }, { "", 0 }, { 0, "" }
};

void fca2 (void)
{
  SA (ca2[0][0] == 0 && ca2[0][1] == 0);
  SA (ca2[1][0] == 0 && ca2[1][1] == 0);
  SA (ca2[2][0] == 0 && ca2[2][1] == 0);

  SA (ca2[3][0] && ca2[3][0][0] == 0 && ca2[3][1] == 0);
  SA (ca2[4][0] && ca2[4][0][0] == 0 && ca2[4][1] && ca2[4][1][0] == 0);
  SA (ca2[5][0] && ca2[5][0][0] == 0 && ca2[5][1] == 0);
  SA (ca2[6][0] == 0 && ca2[6][1] && ca2[6][1][0] == 0);
}

struct A
{
  const char *p;
  char a[2];
};

static constexpr A ca3[] =
{
  {  }, { 0 }, { 0, "" }, { "" }, { "", "" }
};

void fca3 (void)
{
  SA (ca3[0].p == 0 && ca3[0].a[0] == 0 && ca3[0].a[1] == 0);
  SA (ca3[1].p == 0 && ca3[1].a[0] == 0 && ca3[1].a[1] == 0);
  SA (ca3[2].p == 0 && ca3[2].a[0] == 0 && ca3[2].a[1] == 0);
  SA (ca3[3].p && ca3[3].p[0] == 0 && ca3[3].a[0] == 0 && ca3[3].a[1] == 0);
  SA (ca3[4].p && ca3[4].p[0] == 0 && ca3[4].a[0] == 0 && ca3[4].a[1] == 0);
}

#endif   // C++ 11 and above


#define A(e) ((e) ? (void)0 : __builtin_abort ())

static const char* const a1[2] = { "" };

void fa1 (void)
{
  A (a1[0] && a1[0][0] == 0 && a1[1] == 0);
}

static const char* const a2[][2] =
{
  { }, { 0 }, { 0, 0 }, { "" }, { "", "" }, { "", 0 }, { 0, "" }
};

void fa2 (void)
{
  A (a2[0][0] == 0 && a2[0][1] == 0);
  A (a2[1][0] == 0 && a2[1][1] == 0);
  A (a2[2][0] == 0 && a2[2][1] == 0);

  A (a2[3][0] && a2[3][0][0] == 0 && a2[3][1] == 0);
  A (a2[4][0] && a2[4][0][0] == 0 && a2[4][1] && a2[4][1][0] == 0);
  A (a2[5][0] && a2[5][0][0] == 0 && a2[5][1] == 0);
  A (a2[6][0] == 0 && a2[6][1] && a2[6][1][0] == 0);
}

struct B
{
  const char *p;
  char a[2];
};

static const B a3[] =
{
  {  }, { 0 }, { 0, "" }, { "" }, { "", "" }
};

void fa3 (void)
{
  A (a3[0].p == 0 && a3[0].a[0] == 0 && a3[0].a[1] == 0);
  A (a3[1].p == 0 && a3[1].a[0] == 0 && a3[1].a[1] == 0);
  A (a3[2].p == 0 && a3[2].a[0] == 0 && a3[2].a[1] == 0);
  A (a3[3].p && a3[3].p[0] == 0 && a3[3].a[0] == 0 && a3[3].a[1] == 0);
  A (a3[4].p && a3[4].p[0] == 0 && a3[4].a[0] == 0 && a3[4].a[1] == 0);
}
