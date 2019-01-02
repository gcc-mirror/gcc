/* Test to verify that even strictly undefined strlen() calls with
   unterminated character arrays yield the "expected" results when
   the terminating nul is present in a subsequent suobobject.  */

extern __SIZE_TYPE__ strlen (const char *);

unsigned nfails;

#define A(expr, N)						\
  do {								\
    const char *s = (expr);					\
    unsigned n = strlen (s);					\
    ((n == N)							\
     ? 0							\
     : (__builtin_printf ("line %i: strlen (%s = \"%s\")"	\
			  " == %u failed\n",			\
			  __LINE__, #expr, s, N),		\
	++nfails));						\
  } while (0)


int idx;


const char ca[][4] = {
  { '1', '2', '3', '4' }, { '5' },
  { '1', '2', '3', '4' }, { '5', '6' },
  { '1', '2', '3', '4' }, { '5', '6', '7' },
  { '1', '2', '3', '4' }, { '5', '6', '7', '8' },
  { '9' }
};

static void test_const_global_arrays (void)
{
  A (ca[0], 5);
  A (&ca[0][0], 5);
  A (&ca[0][1], 4);
  A (&ca[0][3], 2);

  int i = 0;
  A (ca[i], 5);
  A (&ca[i][0], 5);
  A (&ca[i][1], 4);
  A (&ca[i][3], 2);

  int j = i;
  A (&ca[i][i], 5);
  A (&ca[i][j + 1], 4);
  A (&ca[i][j + 2], 3);

  A (&ca[idx][i], 5);
  A (&ca[idx][j + 1], 4);
  A (&ca[idx][j + 2], 3);

  A (&ca[idx][idx], 5);
  A (&ca[idx][idx + 1], 4);
  A (&ca[idx][idx + 2], 3);

  A (&ca[0][++j], 4);
  A (&ca[0][++j], 3);
  A (&ca[0][++j], 2);

  if (j != 3)
    ++nfails;
}


static void test_const_local_arrays (void)
{
  const char a[][4] = {
    { '1', '2', '3', '4' }, { '5' },
    { '1', '2', '3', '4' }, { '5', '6' },
    { '1', '2', '3', '4' }, { '5', '6', '7' },
    { '1', '2', '3', '4' }, { '5', '6', '7', '8' },
    { '9' }
  };

  A (a[0], 5);
  A (&a[0][0], 5);
  A (&a[0][1], 4);
  A (&a[0][3], 2);

  int i = 0;
  A (a[i], 5);
  A (&a[i][0], 5);
  A (&a[i][1], 4);
  A (&a[i][3], 2);

  int j = i;
  A (&a[i][i], 5);
  A (&a[i][j + 1], 4);
  A (&a[i][j + 2], 3);

  A (&a[idx][i], 5);
  A (&a[idx][j + 1], 4);
  A (&a[idx][j + 2], 3);

  A (&a[idx][idx], 5);
  A (&a[idx][idx + 1], 4);
  A (&a[idx][idx + 2], 3);

  A (&a[0][++j], 4);
  A (&a[0][++j], 3);
  A (&a[0][++j], 2);

  if (j != 3)
    ++nfails;
}


char va[][4] = {
  { '1', '2', '3', '4' }, { '5' },
  { '1', '2', '3', '4' }, { '5', '6' },
  { '1', '2', '3', '4' }, { '5', '6', '7' },
  { '1', '2', '3', '4' }, { '5', '6', '7', '8' },
  { '9' }
};

static void test_nonconst_global_arrays (void)
{
  {
    A (va[0], 5);
    A (&va[0][0], 5);
    A (&va[0][1], 4);
    A (&va[0][3], 2);

    int i = 0;
    A (va[i], 5);
    A (&va[i][0], 5);
    A (&va[i][1], 4);
    A (&va[i][3], 2);

    int j = i;
    A (&va[i][i], 5);
    A (&va[i][j + 1], 4);
    A (&va[i][j + 2], 3);

    A (&va[idx][i], 5);
    A (&va[idx][j + 1], 4);
    A (&va[idx][j + 2], 3);

    A (&va[idx][idx], 5);
    A (&va[idx][idx + 1], 4);
    A (&va[idx][idx + 2], 3);
  }

  {
    A (va[2], 6);
    A (&va[2][0], 6);
    A (&va[2][1], 5);
    A (&va[2][3], 3);

    int i = 2;
    A (va[i], 6);
    A (&va[i][0], 6);
    A (&va[i][1], 5);
    A (&va[i][3], 3);

    int j = i - 1;
    A (&va[i][j - 1], 6);
    A (&va[i][j], 5);
    A (&va[i][j + 1], 4);

    A (&va[idx + 2][i - 1], 5);
    A (&va[idx + 2][j], 5);
    A (&va[idx + 2][j + 1], 4);
  }

  int j = 0;

  A (&va[0][++j], 4);
  A (&va[0][++j], 3);
  A (&va[0][++j], 2);

  if (j != 3)
    ++nfails;
}


static void test_nonconst_local_arrays (void)
{
  char a[][4] = {
    { '1', '2', '3', '4' }, { '5' },
    { '1', '2', '3', '4' }, { '5', '6' },
    { '1', '2', '3', '4' }, { '5', '6', '7' },
    { '1', '2', '3', '4' }, { '5', '6', '7', '8' },
    { '9' }
  };

  A (a[0], 5);
  A (&a[0][0], 5);
  A (&a[0][1], 4);
  A (&a[0][3], 2);

  int i = 0;
  A (a[i], 5);
  A (&a[i][0], 5);
  A (&a[i][1], 4);
  A (&a[i][3], 2);

  int j = i;
  A (&a[i][i], 5);
  A (&a[i][j + 1], 4);
  A (&a[i][j + 2], 3);

  A (&a[idx][i], 5);
  A (&a[idx][j + 1], 4);
  A (&a[idx][j + 2], 3);

  A (&a[idx][idx], 5);
  A (&a[idx][idx + 1], 4);
  A (&a[idx][idx + 2], 3);

  A (&a[0][++j], 4);
  A (&a[0][++j], 3);
  A (&a[0][++j], 2);

  if (j != 3)
    ++nfails;
}


struct MemArrays { char a[4], b[4]; };

const struct MemArrays cma[] = {
  { { '1', '2', '3', '4' }, { '5' } },
  { { '1', '2', '3', '4' }, { '5', '6' } },
  { { '1', '2', '3', '4' }, { '5', '6' } },
  { { '1', '2', '3', '4' }, { '5', '6', '7' } },
  { { '1', '2', '3', '4' }, { '5', '6', '7', '8' } },
  { { '9' }, { '\0' } }
};

static void test_const_global_member_arrays (void)
{
  {
    A (cma[0].a, 5);
    A (&cma[0].a[0], 5);
    A (&cma[0].a[1], 4);
    A (&cma[0].a[2], 3);

    int i = 0;
    A (cma[i].a, 5);
    A (&cma[i].a[0], 5);
    A (&cma[i].a[1], 4);
    A (&cma[i].a[2], 3);

    int j = i;
    A (&cma[i].a[j], 5);
    A (&cma[i].a[j + 1], 4);
    A (&cma[i].a[j + 2], 3);

    A (&cma[idx].a[i], 5);
    A (&cma[idx].a[j + 1], 4);
    A (&cma[idx].a[j + 2], 3);

    A (&cma[idx].a[idx], 5);
    A (&cma[idx].a[idx + 1], 4);
    A (&cma[idx].a[idx + 2], 3);
  }

  {
    A (cma[1].a, 6);
    A (&cma[1].a[0], 6);
    A (&cma[1].a[1], 5);
    A (&cma[1].a[2], 4);

    int i = 1;
    A (cma[i].a, 6);
    A (&cma[i].a[0], 6);
    A (&cma[i].a[1], 5);
    A (&cma[i].a[2], 4);

    int j = i - 1;
    A (&cma[i].a[j], 6);
    A (&cma[i].a[j + 1], 5);
    A (&cma[i].a[j + 2], 4);

    A (&cma[idx + 1].a[j], 6);
    A (&cma[idx + 1].a[j + 1], 5);
    A (&cma[idx + 1].a[j + 2], 4);

    A (&cma[idx + 1].a[idx], 6);
    A (&cma[idx + 1].a[idx + 1], 5);
    A (&cma[idx + 1].a[idx + 2], 4);
  }

  {
    A (cma[4].a, 9);
    A (&cma[4].a[0], 9);
    A (&cma[4].a[1], 8);
    A (&cma[4].b[0], 5);

    int i = 4;
    A (cma[i].a, 9);
    A (&cma[i].a[0], 9);
    A (&cma[i].a[1], 8);
    A (&cma[i].b[0], 5);

    int j = i - 1;
    A (&cma[i].a[j], 6);
    A (&cma[i].a[j + 1], 5);
    A (&cma[i].b[j - 2], 4);

    A (&cma[idx + 4].a[j], 6);
    A (&cma[idx + 4].a[j + 1], 5);
    A (&cma[idx + 4].b[j - 2], 4);

    A (&cma[idx + 4].a[idx], 9);
    A (&cma[idx + 4].a[idx + 1], 8);
    A (&cma[idx + 4].b[idx + 1], 4);
  }
}


static void test_const_local_member_arrays (void)
{
  const struct MemArrays ma[] = {
    { { '1', '2', '3', '4' }, { '5' } },
    { { '1', '2', '3', '4' }, { '5', '6' } },
    { { '1', '2', '3', '4' }, { '5', '6' } },
    { { '1', '2', '3', '4' }, { '5', '6', '7' } },
    { { '1', '2', '3', '4' }, { '5', '6', '7', '8' } },
    { { '9' }, { '\0' } }
  };

  {
    A (ma[0].a, 5);
    A (&ma[0].a[0], 5);
    A (&ma[0].a[1], 4);
    A (&ma[0].a[2], 3);

    int i = 0;
    A (ma[i].a, 5);
    A (&ma[i].a[0], 5);
    A (&ma[i].a[1], 4);
    A (&ma[i].a[2], 3);

    int j = i;
    A (&ma[i].a[j], 5);
    A (&ma[i].a[j + 1], 4);
    A (&ma[i].a[j + 2], 3);

    A (&ma[idx].a[i], 5);
    A (&ma[idx].a[j + 1], 4);
    A (&ma[idx].a[j + 2], 3);

    A (&ma[idx].a[idx], 5);
    A (&ma[idx].a[idx + 1], 4);
    A (&ma[idx].a[idx + 2], 3);
  }

  {
    A (ma[1].a, 6);
    A (&ma[1].a[0], 6);
    A (&ma[1].a[1], 5);
    A (&ma[1].a[2], 4);

    int i = 1;
    A (ma[i].a, 6);
    A (&ma[i].a[0], 6);
    A (&ma[i].a[1], 5);
    A (&ma[i].a[2], 4);

    int j = i - 1;
    A (&ma[i].a[j], 6);
    A (&ma[i].a[j + 1], 5);
    A (&ma[i].a[j + 2], 4);

    A (&ma[idx + 1].a[j], 6);
    A (&ma[idx + 1].a[j + 1], 5);
    A (&ma[idx + 1].a[j + 2], 4);

    A (&ma[idx + 1].a[idx], 6);
    A (&ma[idx + 1].a[idx + 1], 5);
    A (&ma[idx + 1].a[idx + 2], 4);
  }

  {
    A (ma[4].a, 9);
    A (&ma[4].a[0], 9);
    A (&ma[4].a[1], 8);
    A (&ma[4].b[0], 5);

    int i = 4;
    A (ma[i].a, 9);
    A (&ma[i].a[0], 9);
    A (&ma[i].a[1], 8);
    A (&ma[i].b[0], 5);

    int j = i - 1;
    A (&ma[i].a[j], 6);
    A (&ma[i].a[j + 1], 5);
    A (&ma[i].b[j - 2], 4);

    A (&ma[idx + 4].a[j], 6);
    A (&ma[idx + 4].a[j + 1], 5);
    A (&ma[idx + 4].b[j - 2], 4);

    A (&ma[idx + 4].a[idx], 9);
    A (&ma[idx + 4].a[idx + 1], 8);
    A (&ma[idx + 4].b[idx + 1], 4);
  }
}

struct MemArrays vma[] = {
  { { '1', '2', '3', '4' }, { '5' } },
  { { '1', '2', '3', '4' }, { '5', '6' } },
  { { '1', '2', '3', '4' }, { '5', '6' } },
  { { '1', '2', '3', '4' }, { '5', '6', '7' } },
  { { '1', '2', '3', '4' }, { '5', '6', '7', '8' } },
  { { '9' }, { '\0' } }
};

static void test_nonconst_global_member_arrays (void)
{
  {
    A (vma[0].a, 5);
    A (&vma[0].a[0], 5);
    A (&vma[0].a[1], 4);
    A (&vma[0].a[2], 3);

    int i = 0;
    A (vma[i].a, 5);
    A (&vma[i].a[0], 5);
    A (&vma[i].a[1], 4);
    A (&vma[i].a[2], 3);

    int j = i;
    A (&vma[i].a[j], 5);
    A (&vma[i].a[j + 1], 4);
    A (&vma[i].a[j + 2], 3);

    A (&vma[idx].a[i], 5);
    A (&vma[idx].a[j + 1], 4);
    A (&vma[idx].a[j + 2], 3);

    A (&vma[idx].a[idx], 5);
    A (&vma[idx].a[idx + 1], 4);
    A (&vma[idx].a[idx + 2], 3);
  }

  {
    A (vma[1].a, 6);
    A (&vma[1].a[0], 6);
    A (&vma[1].a[1], 5);
    A (&vma[1].a[2], 4);

    int i = 1;
    A (vma[i].a, 6);
    A (&vma[i].a[0], 6);
    A (&vma[i].a[1], 5);
    A (&vma[i].a[2], 4);

    int j = i - 1;
    A (&vma[i].a[j], 6);
    A (&vma[i].a[j + 1], 5);
    A (&vma[i].a[j + 2], 4);

    A (&vma[idx + 1].a[j], 6);
    A (&vma[idx + 1].a[j + 1], 5);
    A (&vma[idx + 1].a[j + 2], 4);

    A (&vma[idx + 1].a[idx], 6);
    A (&vma[idx + 1].a[idx + 1], 5);
    A (&vma[idx + 1].a[idx + 2], 4);
  }

  {
    A (vma[4].a, 9);
    A (&vma[4].a[0], 9);
    A (&vma[4].a[1], 8);
    A (&vma[4].b[0], 5);

    int i = 4;
    A (vma[i].a, 9);
    A (&vma[i].a[0], 9);
    A (&vma[i].a[1], 8);
    A (&vma[i].b[0], 5);

    int j = i - 1;
    A (&vma[i].a[j], 6);
    A (&vma[i].a[j + 1], 5);
    A (&vma[i].b[j - 2], 4);

    A (&vma[idx + 4].a[j], 6);
    A (&vma[idx + 4].a[j + 1], 5);
    A (&vma[idx + 4].b[j - 2], 4);

    A (&vma[idx + 4].a[idx], 9);
    A (&vma[idx + 4].a[idx + 1], 8);
    A (&vma[idx + 4].b[idx + 1], 4);
  }
}


static void test_nonconst_local_member_arrays (void)
{
  struct MemArrays ma[] = {
    { { '1', '2', '3', '4' }, { '5' } },
    { { '1', '2', '3', '4' }, { '5', '6' } },
    { { '1', '2', '3', '4' }, { '5', '6' } },
    { { '1', '2', '3', '4' }, { '5', '6', '7' } },
    { { '1', '2', '3', '4' }, { '5', '6', '7', '8' } },
    { { '9' }, { '\0' } }
  };

  {
    A (ma[0].a, 5);
    A (&ma[0].a[0], 5);
    A (&ma[0].a[1], 4);
    A (&ma[0].a[2], 3);

    int i = 0;
    A (ma[i].a, 5);
    A (&ma[i].a[0], 5);
    A (&ma[i].a[1], 4);
    A (&ma[i].a[2], 3);

    int j = i;
    A (&ma[i].a[j], 5);
    A (&ma[i].a[j + 1], 4);
    A (&ma[i].a[j + 2], 3);

    A (&ma[idx].a[i], 5);
    A (&ma[idx].a[j + 1], 4);
    A (&ma[idx].a[j + 2], 3);

    A (&ma[idx].a[idx], 5);
    A (&ma[idx].a[idx + 1], 4);
    A (&ma[idx].a[idx + 2], 3);
  }

  {
    A (ma[1].a, 6);
    A (&ma[1].a[0], 6);
    A (&ma[1].a[1], 5);
    A (&ma[1].a[2], 4);

    int i = 1;
    A (ma[i].a, 6);
    A (&ma[i].a[0], 6);
    A (&ma[i].a[1], 5);
    A (&ma[i].a[2], 4);

    int j = i - 1;
    A (&ma[i].a[j], 6);
    A (&ma[i].a[j + 1], 5);
    A (&ma[i].a[j + 2], 4);

    A (&ma[idx + 1].a[j], 6);
    A (&ma[idx + 1].a[j + 1], 5);
    A (&ma[idx + 1].a[j + 2], 4);

    A (&ma[idx + 1].a[idx], 6);
    A (&ma[idx + 1].a[idx + 1], 5);
    A (&ma[idx + 1].a[idx + 2], 4);
  }

  {
    A (ma[4].a, 9);
    A (&ma[4].a[0], 9);
    A (&ma[4].a[1], 8);
    A (&ma[4].b[0], 5);

    int i = 4;
    A (ma[i].a, 9);
    A (&ma[i].a[0], 9);
    A (&ma[i].a[1], 8);
    A (&ma[i].b[0], 5);

    int j = i - 1;
    A (&ma[i].a[j], 6);
    A (&ma[i].a[j + 1], 5);
    A (&ma[i].b[j - 2], 4);

    A (&ma[idx + 4].a[j], 6);
    A (&ma[idx + 4].a[j + 1], 5);
    A (&ma[idx + 4].b[j - 2], 4);

    A (&ma[idx + 4].a[idx], 9);
    A (&ma[idx + 4].a[idx + 1], 8);
    A (&ma[idx + 4].b[idx + 1], 4);
  }
}


union UnionMemberArrays
{
  struct { char a[4], b[4]; } a;
  struct { char a[8]; } c;
};

const union UnionMemberArrays cu = {
  { { '1', '2', '3', '4' }, { '5', } }
};

static void test_const_union_member_arrays (void)
{
  A (cu.a.a, 5);
  A (cu.a.b, 1);
  A (cu.c.a, 5);

  const union UnionMemberArrays clu = {
    { { '1', '2', '3', '4' }, { '5', '6' } }
  };

  A (clu.a.a, 6);
  A (clu.a.b, 2);
  A (clu.c.a, 6);
}


union UnionMemberArrays vu = {
  { { '1', '2', '3', '4' }, { '5', '6' } }
};

static void test_nonconst_union_member_arrays (void)
{
  A (vu.a.a, 6);
  A (vu.a.b, 2);
  A (vu.c.a, 6);

  union UnionMemberArrays lvu = {
    { { '1', '2', '3', '4' }, { '5', '6', '7' } }
  };

  A (lvu.a.a, 7);
  A (lvu.a.b, 3);
  A (lvu.c.a, 7);
}


int main (void)
{
  test_const_global_arrays ();
  test_const_local_arrays ();

  test_nonconst_global_arrays ();
  test_nonconst_local_arrays ();

  test_const_global_member_arrays ();
  test_const_local_member_arrays ();

  test_nonconst_global_member_arrays ();
  test_nonconst_local_member_arrays ();

  test_const_union_member_arrays ();
  test_nonconst_union_member_arrays ();

  if (nfails)
    __builtin_abort ();
}
