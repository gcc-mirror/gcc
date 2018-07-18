/* -*-c-*- */

#undef NEW
#define NEW 3

__attribute__ ((noinline))
int FN(_bo)(TYPE *mem, TYPE *old_ret, TYPE old)
{
  *old_ret = old;
  return __atomic_compare_exchange_n (mem, (void *)old_ret, NEW, 1, 2, 0);
}

__attribute__ ((noinline))
void FN(_o)(TYPE *mem, TYPE *old_ret, TYPE old)
{
  *old_ret = old;
  __atomic_compare_exchange_n (mem, (void *)old_ret, NEW, 1, 2, 0);
  return;
}

__attribute__ ((noinline))
int FN(_b)(TYPE *mem, TYPE old)
{
  return __atomic_compare_exchange_n (mem, (void *)&old, NEW, 1, 2, 0);
}

__attribute__ ((noinline))
void FN()(TYPE *mem, TYPE old)
{
  __atomic_compare_exchange_n (mem, (void *)&old, NEW, 1, 2, 0);
  return;
}

/* Const != 0 old value.  */
__attribute__ ((noinline))
int FN(_c1_bo)(TYPE *mem, TYPE *old_ret)
{
  *old_ret = 1;
  return __atomic_compare_exchange_n (mem, (void *)old_ret, NEW, 1, 2, 0);
}

__attribute__ ((noinline))
void FN(_c1_o)(TYPE *mem, TYPE *old_ret)
{
  *old_ret = 1;
  __atomic_compare_exchange_n (mem, (void *)old_ret, NEW, 1, 2, 0);
  return;
}

__attribute__ ((noinline))
int FN(_c1_b)(TYPE *mem)
{
  TYPE old = 1;
  return __atomic_compare_exchange_n (mem, (void *)&old, NEW, 1, 2, 0);
}

__attribute__ ((noinline))
void FN(_c1)(TYPE *mem)
{
  TYPE old = 1;
  __atomic_compare_exchange_n (mem, (void *)&old, NEW, 1, 2, 0);
  return;
}

/* Const == 0 old value.  */
__attribute__ ((noinline))
int FN(_c0_bo)(TYPE *mem, TYPE *old_ret)
{
  *old_ret = 0;
  return __atomic_compare_exchange_n (mem, (void *)old_ret, NEW, 1, 2, 0);
}

__attribute__ ((noinline))
void FN(_c0_o)(TYPE *mem, TYPE *old_ret)
{
  *old_ret = 0;
  __atomic_compare_exchange_n (mem, (void *)old_ret, NEW, 1, 2, 0);
  return;
}

__attribute__ ((noinline))
int FN(_c0_b)(TYPE *mem)
{
  TYPE old = 0;
  return __atomic_compare_exchange_n (mem, (void *)&old, NEW, 1, 2, 0);
}

__attribute__ ((noinline))
void FN(_c0)(TYPE *mem)
{
  TYPE old = 0;
  __atomic_compare_exchange_n (mem, (void *)&old, NEW, 1, 2, 0);
  return;
}

int FN(_validate_mem)(TYPE *mem, TYPE expected_mem)
{
  if (*mem != expected_mem)
    {
      fprintf(stderr, "  BAD: mem %d != expected mem %d\n",
	      *mem, expected_mem);
      return 1;
    }

  return 0;
}

int FN(_validate_rc)(int rc, int expected_rc)
{
  if (rc != expected_rc)
    {
      fprintf(stderr, "  BAD: rc %d != expected rc %d\n",
	      rc, expected_rc);
      return 1;
    }

  return 0;
}

int FN(_validate_old_ret)(int old_ret, int expected_old_ret)
{
  if (old_ret != expected_old_ret)
    {
      fprintf(stderr, "  BAD: old_ret %d != expected old_ret %d\n",
	      old_ret, expected_old_ret);
      return 1;
    }

  return 0;
}

int FN(_validate)(TYPE *mem, TYPE init_mem, TYPE old)
{
  int err_count = 0;
  int rc;
  TYPE expected_mem;
  int expected_rc;
  TYPE old_ret;
  int failed;
  const char *fname;

  fprintf(stderr, "%s: init_mem %d @ %p\n", __FUNCTION__, init_mem, mem);
  {
    failed = 0;
    old = 1;
    *mem = init_mem;
    expected_mem = (init_mem == old) ? NEW : *mem;
    expected_rc = (init_mem == old);
    fname = FNS(_bo);
    rc = FN(_bo)(mem, &old_ret, old);
    failed |= FN(_validate_rc)(rc, expected_rc);
    failed |= FN(_validate_old_ret)(old_ret, init_mem);
    failed |= FN(_validate_mem)(mem, expected_mem);
    if (failed)
      {
	fprintf(stderr, "  FAIL: %s: near line %d\n", fname, __LINE__ - 3);
	err_count++;
      }
  }
  {
    failed = 0;
    old = 1;
    *mem = init_mem;
    expected_mem = (init_mem == old) ? NEW : *mem;
    fname = FNS(_o);
    FN(_o)(mem, &old_ret, old);
    failed |= FN(_validate_old_ret)(old_ret, init_mem);
    failed |= FN(_validate_mem)(mem, expected_mem);
    if (failed)
      {
	fprintf(stderr, "  FAIL: %s: near line %d\n", fname, __LINE__ - 3);
	err_count++;
      }
  }
  {
    failed = 0;
    old = 1;
    *mem = init_mem;
    expected_mem = (init_mem == old) ? NEW : *mem;
    expected_rc = (init_mem == old);
    fname = FNS(_b);
    rc = FN(_b)(mem, old);
    failed |= FN(_validate_rc)(rc, expected_rc);
    failed |= FN(_validate_mem)(mem, expected_mem);
    if (failed)
      {
	fprintf(stderr, "  FAIL: %s: near line %d\n", fname, __LINE__ - 3);
	err_count++;
      }
  }
  {
    failed = 0;
    old = 1;
    *mem = init_mem;
    expected_mem = (init_mem == old) ? NEW : *mem;
    fname = FNS();
    FN()(mem, old);
    failed |= FN(_validate_mem)(mem, expected_mem);
    if (failed)
      {
	fprintf(stderr, "  FAIL: %s: near line %d\n", fname, __LINE__ - 3);
	err_count++;
      }
  }
  {
    failed = 0;
    old = 1;
    *mem = init_mem;
    expected_mem = (init_mem == old) ? NEW : *mem;
    expected_rc = (init_mem == old);
    fname = FNS(_c1_bo);
    rc = FN(_c1_bo)(mem, &old_ret);
    failed |= FN(_validate_rc)(rc, expected_rc);
    failed |= FN(_validate_old_ret)(old_ret, init_mem);
    failed |= FN(_validate_mem)(mem, expected_mem);
    if (failed)
      {
	fprintf(stderr, "  FAIL: %s: near line %d\n", fname, __LINE__ - 3);
	err_count++;
      }
  }
  {
    failed = 0;
    old = 1;
    *mem = init_mem;
    expected_mem = (init_mem == old) ? NEW : *mem;
    fname = FNS(_c1_o);
    FN(_c1_o)(mem, &old_ret);
    failed |= FN(_validate_old_ret)(old_ret, init_mem);
    failed |= FN(_validate_mem)(mem, expected_mem);
    if (failed)
      {
	fprintf(stderr, "  FAIL: %s: near line %d\n", fname, __LINE__ - 3);
	err_count++;
      }
  }
  {
    failed = 0;
    old = 1;
    *mem = init_mem;
    expected_mem = (init_mem == old) ? NEW : *mem;
    expected_rc = (init_mem == old);
    fname = FNS(_c1_b);
    rc = FN(_c1_b)(mem);
    failed |= FN(_validate_rc)(rc, expected_rc);
    failed |= FN(_validate_mem)(mem, expected_mem);
    if (failed)
      {
	fprintf(stderr, "  FAIL: %s: near line %d\n", fname, __LINE__ - 3);
	err_count++;
      }
  }
  {
    failed = 0;
    old = 1;
    *mem = init_mem;
    expected_mem = (init_mem == old) ? NEW : *mem;
    fname = FNS(_c1);
    FN(_c1)(mem);
    failed |= FN(_validate_mem)(mem, expected_mem);
    if (failed)
      {
	fprintf(stderr, "  FAIL: %s: near line %d\n", fname, __LINE__ - 3);
	err_count++;
      }
  }
  {
    failed = 0;
    old = 0;
    *mem = init_mem;
    expected_mem = (init_mem == old) ? NEW : *mem;
    expected_rc = (init_mem == old);
    fname = FNS(_c0_bo);
    rc = FN(_c0_bo)(mem, &old_ret);
    failed |= FN(_validate_rc)(rc, expected_rc);
    failed |= FN(_validate_old_ret)(old_ret, init_mem);
    failed |= FN(_validate_mem)(mem, expected_mem);
    if (failed)
      {
	fprintf(stderr, "  FAIL: %s: near line %d\n", fname, __LINE__ - 3);
	err_count++;
      }
  }
  {
    failed = 0;
    old = 0;
    *mem = init_mem;
    expected_mem = (init_mem == old) ? NEW : *mem;
    fname = FNS(_c0_o);
    FN(_c0_o)(mem, &old_ret);
    failed |= FN(_validate_old_ret)(old_ret, init_mem);
    failed |= FN(_validate_mem)(mem, expected_mem);
    if (failed)
      {
	fprintf(stderr, "  FAIL: %s: near line %d\n", fname, __LINE__ - 3);
	err_count++;
      }
  }
  {
    failed = 0;
    old = 0;
    *mem = init_mem;
    expected_mem = (init_mem == old) ? NEW : *mem;
    expected_rc = (init_mem == old);
    fname = FNS(_c0_b);
    rc = FN(_c0_b)(mem);
    failed |= FN(_validate_rc)(rc, expected_rc);
    failed |= FN(_validate_mem)(mem, expected_mem);
    if (failed)
      {
	fprintf(stderr, "  FAIL: %s: near line %d\n", fname, __LINE__ - 3);
	err_count++;
      }
  }
  {
    failed = 0;
    old = 0;
    *mem = init_mem;
    expected_mem = (init_mem == old) ? NEW : *mem;
    fname = FNS(_c0);
    FN(_c0)(mem);
    failed |= FN(_validate_mem)(mem, expected_mem);
    if (failed)
      {
	fprintf(stderr, "  FAIL: %s: near line %d\n", fname, __LINE__ - 3);
	err_count++;
      }
  }

  return err_count;
}

#undef TYPE
#undef MEM
#undef FN
#undef FNS
