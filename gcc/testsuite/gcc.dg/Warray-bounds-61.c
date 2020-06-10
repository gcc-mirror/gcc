/* PR middle-end/94940 - spurious -Warray-bounds for a zero length array
   member of union
   { dg-do compile }
   { dg-options "-O2 -Wall" } */

extern int n;

extern union Ua3_a0 {
  int a3[3];
  int a2[2];                  // can only alias a3[0 - 2]
  int a1[1];                  // can alias all of the union
  int a0[0];                  // ditto
} ua3_a0;

void test_ua3_ua0_a0 (int i)
{
  ua3_a0.a0[0] = 0;           // { dg-bogus "\\\[-Warray-bounds" }
  ua3_a0.a0[1] = 0;           // { dg-bogus "\\\[-Warray-bounds" }
  ua3_a0.a0[2] = 0;           // { dg-bogus "\\\[-Warray-bounds" }
  ua3_a0.a0[3] = 0;           // { dg-warning "\\\[-Warray-bounds" }
  ua3_a0.a0[4] = 0;           // { dg-warning "\\\[-Warray-bounds" }
  ua3_a0.a0[i] = 0;           // { dg-bogus "\\\[-Warray-bounds" }

  if (i < __LINE__)
    i = 5;
  ua3_a0.a0[i] = 0;           // { dg-warning "\\\[-Warray-bounds" }

  if (i > -1)
    i = -1;
  ua3_a0.a0[i] = 0;           // { dg-warning "\\\[-Warray-bounds" }
}

void test_ua3_ua0_a1 (int i)
{
  /* Abusing one-element array members the same way as those of
     length zero is discouraged but so far acceted without warnings.
     This should change at some point.  */

  ua3_a0.a1[0] = 0;
  ua3_a0.a1[1] = 0;
  ua3_a0.a1[2] = 0;
  ua3_a0.a1[3] = 0;           // { dg-warning "\\\[-Warray-bounds" }
  ua3_a0.a1[i] = 0;

  if (i > -1)
    i = -1;
  ua3_a0.a1[i] = 0;           // { dg-warning "\\\[-Warray-bounds" }

  if (i < 7)
    i = 7;
  ua3_a0.a1[i] = 0;           // { dg-warning "\\\[-Warray-bounds" }
}

void test_ua3_ua0_a2 (int i)
{
  ua3_a0.a2[0] = 0;
  ua3_a0.a2[1] = 0;
  ua3_a0.a2[2] = 0;           // { dg-warning "\\\[-Warray-bounds" }
  ua3_a0.a2[i] = 0;

  if (i < __LINE__)
    i = __LINE__;
  ua3_a0.a2[i] = 0;           // { dg-warning "\\\[-Warray-bounds" }

  if (i > -1)
    i = -1;
  ua3_a0.a2[i] = 0;           // { dg-warning "\\\[-Warray-bounds" }
}


extern union Ua2_a3 {
  int a2[2];                  // can only alias a3[0 - 1]
  int a3[3];
} ua2_a3;

void test_ua2_ua3 (int i)
{
  ua2_a3.a2[0] = 0;           // { dg-bogus "\\\[-Warray-bounds" }
  ua2_a3.a2[1] = 0;           // { dg-bogus "\\\[-Warray-bounds" }
  ua2_a3.a2[2] = 0;           // { dg-warning "\\\[-Warray-bounds" }
  ua2_a3.a2[i] = 0;

  if (i < __LINE__)
    i = __LINE__;
  ua2_a3.a2[i] = 0;           // { dg-warning "\\\[-Warray-bounds" }
}


extern struct SUa2_a0 {
  union Ua2_a0 {
    int a2[2];
    int a0[0];
  } u;
} sua2_a0;

void test_sua2_sua0 (int i)
{
  n += sua2_a0.u.a0[0];
  n += sua2_a0.u.a0[1];
  n += sua2_a0.u.a0[2];       // { dg-warning "\\\[-Warray-bounds" }
  n += sua2_a0.u.a0[i];

  if (i < __LINE__)
    i = __LINE__;
  n += sua2_a0.u.a0[i];       // { dg-warning "\\\[-Warray-bounds" }
}

void test_sua2_sua0_ptr (int i)
{
  union Ua2_a0 *p = &sua2_a0.u;

  n += p->a0[0];
  n += p->a0[1];
  n += p->a0[2];              // { dg-warning "\\\[-Warray-bounds" }
  n += p->a0[i];
}


extern struct SUSa3_a0 {
  union USa3_a0 {
    struct {
      int a3[3];
    } s;
    int a2[2];                // can alias s.a3[0 - 2]
    int a1[1];                // can alias s.a3[0 - 2]
    int a0[0];                // can alias s.a3[0]
  } u;
} susa3_ua0;

void test_susa3_sua0 (int i, int j)
{
  n += susa3_ua0.u.a0[0];
  n += susa3_ua0.u.a0[1];
  n += susa3_ua0.u.a0[2];
  n += susa3_ua0.u.a0[3];     // { dg-warning "\\\[-Warray-bounds" }
}

void test_susa3_sua0_ptr (int i, int j)
{
  union USa3_a0 *p = &susa3_ua0.u;
  n += p->a0[0];
  n += p->a0[1];
  n += p->a0[2];
  n += p->a0[3];              // { dg-warning "\\\[-Warray-bounds" }
}

void test_susa3_sua1 (int i)
{
  n += susa3_ua0.u.a1[0];
  n += susa3_ua0.u.a1[1];
  n += susa3_ua0.u.a1[2];
  n += susa3_ua0.u.a1[3];     // { dg-warning "\\\[-Warray-bounds" }

  if (i < __LINE__)
    i = __LINE__;
  n += susa3_ua0.u.a1[i];     // { dg-warning "\\\[-Warray-bounds" }
}

void test_susa3_sua2 (void)
{
  n += susa3_ua0.u.a2[0];
  n += susa3_ua0.u.a2[1];
  n += susa3_ua0.u.a2[2];     // { dg-warning "\\\[-Warray-bounds" }
  n += susa3_ua0.u.a2[3];     // { dg-warning "\\\[-Warray-bounds" }
}


extern struct {
  union {
    struct {
      int a3[3];
    } s1;
    struct {
      int a0[0];
    } s2;
  } u;
} susa3_usa0;

void test_susi3_susi0 (int i)
{
  n += susa3_usa0.u.s2.a0[0];
  n += susa3_usa0.u.s2.a0[1];
  n += susa3_usa0.u.s2.a0[2];
  n += susa3_usa0.u.s2.a0[3]; // { dg-warning "\\\[-Warray-bounds" }
  n += susa3_usa0.u.s2.a0[i];

  if (i < __LINE__)
    i = __LINE__;
  n += susa3_usa0.u.s2.a0[i]; // { dg-warning "\\\[-Warray-bounds" }
}
