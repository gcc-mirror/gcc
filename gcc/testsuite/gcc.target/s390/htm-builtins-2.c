/* Functional tests of the htm __TM_... macros.  */

/* { dg-do run } */
/* { dg-require-effective-target htm } */
/* { dg-options "-O3 -march=zEC12 -mzarch" } */

/* ---------------------------- included header files ---------------------- */

#include <stdio.h>
#include <string.h>
#include <inttypes.h>
#include <htmxlintrin.h>

/* ---------------------------- local definitions -------------------------- */

#define DEFAULT_MAX_REPETITIONS 7
#define DEFAULT_REQUIRED_QUORUM 4
#define DEFAULT_ABORT_ADDRESS (0x12345678u)

/* ---------------------------- local macros ------------------------------- */

#define TEST_DF_REP(name) \
  { #name, name, DEFAULT_MAX_REPETITIONS, DEFAULT_REQUIRED_QUORUM }
#define TEST_NO_REP(name) { #name, name, 1, 1 }

/* ---------------------------- local types -------------------------------- */

typedef int (*test_func_t)(void);

typedef struct
{
  const char *name;
  test_func_t test_func;
  int max_repetitions;
  int required_quorum;
} test_table_entry_t;

typedef enum
{
  ABORT_T_SYSTEM = 0,
  ABORT_T_USER = 1,
} abort_user_t;

typedef enum
{
  ABORT_T_NONE = 0,
  ABORT_T_ILLEGAL,
  ABORT_T_FOOTPRINT_EXCEEDED,
  ABORT_T_NESTED_TOO_DEEP,
  ABORT_T_CONFLICT,

  ABORT_T_INVALID_ABORT_CODE
} abort_t;

/* ---------------------------- local variables ---------------------------- */

__attribute__ ((aligned(256))) static struct __htm_tdb local_tdb256;
static struct __htm_tdb local_tdb;

static abort_t const abort_classes[] =
{
  ABORT_T_INVALID_ABORT_CODE,
  ABORT_T_NONE,
  ABORT_T_NONE,
  ABORT_T_NONE,

  ABORT_T_ILLEGAL,
  ABORT_T_NONE,
  ABORT_T_NONE,
  ABORT_T_FOOTPRINT_EXCEEDED,

  ABORT_T_FOOTPRINT_EXCEEDED,
  ABORT_T_CONFLICT,
  ABORT_T_CONFLICT,
  ABORT_T_ILLEGAL,

  ABORT_T_NONE,
  ABORT_T_NESTED_TOO_DEEP,
  ABORT_T_NONE,
  ABORT_T_NONE,

  ABORT_T_NONE
};

static size_t num_abort_classes = sizeof(abort_classes) / sizeof(abort_t);

/* ---------------------------- exported variables (globals) --------------- */

int global_int = 0;
uint64_t global_u64 = 0;
float global_float_1 = 1.0;
float global_float_2 = 2.5;
float global_float_3 = 0.0;
__attribute__ ((aligned(256))) struct
{
  volatile uint64_t c1;
  char pad1[256 - sizeof(uint64_t)];
  volatile uint64_t c2;
  char pad2[256 - sizeof(uint64_t)];
  volatile uint64_t c3;
} counters = { 0 };

/* ---------------------------- local helper functions --------------------- */

static void dump_tdb(struct __htm_tdb *tdb)
{
  unsigned char *p;
  int i;
  int j;

  p = (unsigned char *)tdb;
  for (i = 0; i < 16; i++)
    {
      fprintf(stderr, "0x%02x  ", i * 16);
      for (j = 0; j < 16; j++)
	{
	  fprintf(stderr, "%02x", (int)p[i * 16 + j]);
	  if (j < 15)
	    {
	      fprintf(stderr, " ");
	    }
	  if (j == 7)
	    {
	      fprintf(stderr, " ");
	    }
	}
      fprintf(stderr, "\n");
    }

  return;
}

static void make_fake_tdb(struct __htm_tdb *tdb)
{
  memset(tdb, 0, sizeof(*tdb));
  tdb->format = 1;
  tdb->nesting_depth = 1;
  tdb->atia = DEFAULT_ABORT_ADDRESS;
  tdb->abort_code = 11;

  return;
}

static int check_abort_code_in_tdb(struct __htm_tdb *tdb, uint64_t abort_code)
{
  long expect_rc;
  long rc;

  if (abort_code != 0)
    {
      long addr;

      addr = __TM_failure_address(&local_tdb);
      if (addr != DEFAULT_ABORT_ADDRESS)
	{
	  return 11;
	}
    }
  {
    long long tdb_abort_code;

    tdb_abort_code = __TM_failure_code(tdb);
    if ((uint64_t)tdb_abort_code != abort_code)
      {
	fprintf(
		stderr, "tm_ac %" PRIu64 ", ac %" PRIu64
		", tdb_ac %" PRIu64 "\n",
		(uint64_t)tdb_abort_code, abort_code,
		(uint64_t)tdb->abort_code);
	return 10;
      }
  }
  expect_rc = (abort_code >= 256) ? 1 : 0;
  rc = __TM_is_user_abort(tdb);
  if (rc != expect_rc)
    {
      fprintf(stderr, "rc %ld, expect_rc %ld\n", rc, expect_rc);
      return 1;
    }
  {
    unsigned char code;

    code = 0xffu;
    rc = __TM_is_named_user_abort(tdb, &code);
    if (rc != expect_rc)
      {
	fprintf(
		stderr, "rc %ld, expect_rc %ld\n", rc,
		expect_rc);
	return 2;
      }
    if (expect_rc == 1 && code != abort_code - 256)
      {
	return 3;
      }
  }
  if (abort_code > (uint64_t)num_abort_classes)
    {
      abort_code = (uint64_t)num_abort_classes;
    }
  expect_rc = (abort_classes[abort_code] == ABORT_T_ILLEGAL) ? 1 : 0;
  rc = __TM_is_illegal(tdb);
  if (rc != expect_rc)
    {
      dump_tdb(tdb);
      fprintf(stderr, "rc %ld, expect_rc %ld\n", rc, expect_rc);
      return 4;
    }
  expect_rc =
    (abort_classes[abort_code] == ABORT_T_FOOTPRINT_EXCEEDED) ?
    1 : 0;
  rc = __TM_is_footprint_exceeded(tdb);
  if (rc != expect_rc)
    {
      dump_tdb(tdb);
      fprintf(stderr, "rc %ld, expect_rc %ld\n", rc, expect_rc);
      return 5;
    }
  expect_rc =
    (abort_classes[abort_code] == ABORT_T_NESTED_TOO_DEEP) ? 1 : 0;
  rc = __TM_is_nested_too_deep(tdb);
  if (rc != expect_rc)
    {
      dump_tdb(tdb);
      fprintf(stderr, "rc %ld, expect_rc %ld\n", rc, expect_rc);
      return 6;
    }
  expect_rc = (abort_classes[abort_code] == ABORT_T_CONFLICT) ? 1 : 0;
  rc = __TM_is_conflict(tdb);
  if (rc != expect_rc)
    {
      dump_tdb(tdb);
      fprintf(stderr, "rc %ld, expect_rc %ld\n", rc, expect_rc);
      return 7;
    }

  return 0;
}

/* ---------------------------- local test functions ----------------------- */

/* Not a test; make sure that the involved global cachelines are reserved for
 * writing.  */
static int init_cache(void)
{
  make_fake_tdb(&local_tdb);
  make_fake_tdb(&local_tdb256);
  global_int = 0;
  global_u64 = 0;
  global_float_1 = 1.0;
  global_float_2 = 2.5;
  global_float_3 = 0.0;
  counters.c1 = 0;
  counters.c2 = 0;
  counters.c3 = 0;

  return 0;
}

static int test_abort_classification(void)
{
  int i;

  make_fake_tdb(&local_tdb);
  for (i = 0; i <= 256; i++)
    {
      int rc;

      local_tdb.abort_code = (uint64_t)i;
      rc = check_abort_code_in_tdb(&local_tdb, (uint64_t)i);
      if (rc != 0)
	{
	  return 100 * i + rc;
	}
    }

  return 0;
}

static int test_cc_classification(void)
{
  long rc;

  rc = __TM_is_failure_persistent(0);
  if (rc != 0)
    {
      return 1;
    }
  rc = __TM_is_failure_persistent(1);
  if (rc != 0)
    {
      return 2;
    }
  rc = __TM_is_failure_persistent(2);
  if (rc != 0)
    {
      return 3;
    }
  rc = __TM_is_failure_persistent(3);
  if (rc != 1)
    {
      return 4;
    }

  return 0;
}

static int test_tbegin_ntstg_tend(void)
{
  long rc;

  counters.c1 = 0;
  counters.c2 = 0;
  if ((rc = __TM_simple_begin()) == 0)
    {
      __TM_non_transactional_store((uint64_t *)&counters.c1, 1);
      counters.c2 = 2;
      rc = __TM_end();
      if (rc != 0)
	{
	  return 100 * rc + 5;
	}
      if (counters.c1 != 1)
	{
	  return 100 * counters.c1 + 2;
	}
      if (counters.c2 != 2)
	{
	  return 100 * counters.c2 + 3;
	}
    }
  else
    {
      return 100 * rc + 4;
    }

  return 0;
}

static int test_tbegin_ntstg_tabort(void)
{
  register float f;

  counters.c1 = 0;
  counters.c2 = 0;
  f = 0;
  if (__TM_simple_begin() == 0)
    {
      __TM_non_transactional_store((uint64_t *)&counters.c1, 1);
      counters.c2 = 2;
      f = 1;
      __TM_named_abort(0);
      return 1;
    }
  if (counters.c1 != 1)
    {
      return 100 * counters.c1 + 2;
    }
  if (counters.c2 != 0)
    {
      return 100 * counters.c2 + 3;
    }
  if (f != 0)
    {
      return 100 * f + 4;
    }

  return 0;
}

static int test_tbegin_aborts(void)
{
  float f;
  long rc;

  f = 77;
  if ((rc = __TM_simple_begin()) == 0)
    {
      f = 88;
      __TM_abort();
      return 2;
    }
  else if (rc != 2)
    {
      return 3;
    }
  if (f != 77)
    {
      return 4;
    }
  f = 66;
  if ((rc = __TM_simple_begin()) == 0)
    {
      f = 99;
      __TM_named_abort(3);
      return 5;
    }
  else if (rc != 3)
    {
      return 100 * rc + 6;
    }
  if (f != 66)
    {
      return 100 * f + 7;
    }
  if ((rc = __TM_simple_begin()) == 0)
    {
      global_float_3 = global_float_1 + global_float_2;
      rc = __TM_end();
      if (rc != 0)
	{
	  return 100 * rc + 8;
	}
    }
  else
    {
      return 100 * rc + 9;
    }
  if (global_float_3 != global_float_1 + global_float_2)
    {
      return 100 * rc + 10;
    }

  return 0;
}

static int test_tbegin_tdb(void)
{
  long rc;

  local_tdb.format = 0;
  if ((rc = __TM_begin(&local_tdb)) == 0)
    {
      rc = __TM_end();
      if (rc != 0)
	{
	  return 100 * rc + 1;
	}
      if (local_tdb.format != 0)
	{
	  dump_tdb(&local_tdb);
	  return 100 * local_tdb.format + 2;
	}
    }
  else
    {
      return 100 * rc + 3;
    }
  local_tdb.format = 0;
  if ((rc = __TM_begin(&local_tdb)) == 0)
    {
      __TM_named_abort(1);
      return 4;
    }
  else
    {
      if (rc != 3)
	{
	  return 100 * rc + 5;
	}
      if (local_tdb.format != 1)
	{
	  dump_tdb(&local_tdb);
	  return 100 * local_tdb.format + 6;
	}
    }
  local_tdb256.format = 0;
  if ((rc = __TM_begin(&local_tdb256)) == 0)
    {
      rc = __TM_end();
      if (rc != 0)
	{
	  return 1100 * rc + 1;
	}
      if (local_tdb256.format != 0)
	{
	  dump_tdb(&local_tdb256);
	  return 1100 * local_tdb256.format + 2;
	}
    }
  else
    {
      return 1100 * rc + 3;
    }
#if 1 /*!!!does not work*/
  local_tdb256.format = 0;
  if ((rc = __TM_begin(&local_tdb256)) == 0)
    {
      __TM_named_abort(1);
      return 2004;
    }
  else
    {
      if (rc != 3)
	{
	  return 2100 * rc + 5;
	}
      if (local_tdb256.format != 1)
	{
	  dump_tdb(&local_tdb256);
	  return 2100 * local_tdb256.format + 6;
	}
    }
#endif

  return 0;
}

static int test_etnd(void)
{
  long rc;

  {
    long nd;

    make_fake_tdb(&local_tdb);
    local_tdb.nesting_depth = 0;
    nd = __TM_nesting_depth(&local_tdb);
    if (nd != 0)
      {
	return 1;
      }
    local_tdb.nesting_depth = 7;
    nd = __TM_nesting_depth(&local_tdb);
    if (nd != 7)
      {
	return 7;
      }
    local_tdb.format = 0;
    nd = __TM_nesting_depth(&local_tdb);
    if (nd != 0)
      {
	return 2;
      }
  }
  counters.c1 = 0;
  counters.c1 = 0;
  counters.c2 = 0;
  counters.c3 = 0;
  if ((rc = __TM_simple_begin()) == 0)
    {
      counters.c1 = __TM_nesting_depth(0);
      if (__TM_simple_begin() == 0)
	{
	  counters.c2 = __TM_nesting_depth(0);
	  if (__TM_simple_begin() == 0)
	    {
	      counters.c3 = __TM_nesting_depth(0);
	      __TM_end();
	    }
	  __TM_end();
	}
      __TM_end();
    }
  else
    {
      return 100 * rc + 1;
    }
  if (counters.c1 != 1)
    {
      return 100 * counters.c1 + 2;
    }
  if (counters.c2 != 2)
    {
      return 100 * counters.c2 + 3;
    }
  if (counters.c3 != 3)
    {
      return 100 * counters.c3 + 4;
    }

  return 0;
}

/* ---------------------------- local testing framework functions ---------- */

static int run_one_test(const test_table_entry_t *test_entry)
{
  int do_print_passes;
  int succeeded;
  int rc;
  int i;

  do_print_passes = (
		     test_entry->required_quorum != 1 ||
		     test_entry->max_repetitions != 1);
  printf("RRR RUN  %s\n", test_entry->name);
  if (do_print_passes == 1)
    {
      printf(
	     "         (requires %d successful out of %d runs)\n",
	     test_entry->required_quorum,
	     test_entry->max_repetitions);
    }
  succeeded = 0;
  rc = 0;
  for (rc = 0, i = 0; i < test_entry->max_repetitions; i++)
    {
      if (do_print_passes == 1)
	{
	  if (i == 0)
	    {
	      printf("        ");
	    }
	  else
	    {
	      printf(",");
	    }
	}
      rc = test_entry->test_func();
      if (rc == 0)
	{
	  if (do_print_passes == 1)
	    {
	      printf(" success");
	    }
	  succeeded++;
	  if (succeeded >= test_entry->required_quorum)
	    {
	      break;
	    }
	}
      else
	{
	  printf(" failed (rc = %d)", rc);
	}
    }
  if (do_print_passes == 1 || rc != 0)
    {
      printf("\n");
    }
  if (succeeded >= test_entry->required_quorum)
    {
      printf("+++ OK   %s\n", test_entry->name);

      return 0;
    }
  else
    {
      printf("--- FAIL %s\n", test_entry->name);

      return (rc != 0) ? rc : -1;
    }
}

static int run_all_tests(const test_table_entry_t *test_table)
{
  const test_table_entry_t *test;
  int rc;

  for (
       rc = 0, test = &test_table[0];
       test->test_func != NULL && rc == 0; test++)
    {
      rc = run_one_test(test);
    }

  return rc;
}

/* ---------------------------- interface functions ------------------------ */

int main(void)
{
  const test_table_entry_t test_table[] = {
    TEST_NO_REP(init_cache),
    TEST_NO_REP(test_abort_classification),
    TEST_NO_REP(test_cc_classification),
    TEST_DF_REP(test_tbegin_ntstg_tend),
    TEST_DF_REP(test_tbegin_ntstg_tabort),
    TEST_DF_REP(test_tbegin_aborts),
    TEST_DF_REP(test_tbegin_tdb),
    TEST_DF_REP(test_etnd),
    { (void *)0, 0, 0 }
  };

  {
    int rc;

    rc = run_all_tests(test_table);

    return rc;
  }
}
