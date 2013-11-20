/* Functional tests of the htm __builtin_... macros.  */

/* { dg-do run } */
/* { dg-require-effective-target htm } */
/* { dg-options "-O3 -march=zEC12 -mzarch" } */

/* ---------------------------- included header files ---------------------- */

#include <stdio.h>
#include <string.h>
#include <stdint.h>
#include <htmintrin.h>

/* ---------------------------- local definitions -------------------------- */

#define DEFAULT_MAX_REPETITIONS 5
#define DEFAULT_REQUIRED_QUORUM ((DEFAULT_MAX_REPETITIONS) - 1)
#define NUM_WARMUP_RUNS 10

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

/* ---------------------------- local variables ---------------------------- */

__attribute__ ((aligned(256))) static struct __htm_tdb local_tdb256;
static struct __htm_tdb local_tdb;
static int do_dump_tdb = 0;

/* ---------------------------- exported variables (globals) --------------- */

__attribute__ ((aligned(256))) struct
{
  float float_1;
  float float_2;
  float float_3;
} global = { 1.0, 2.5, 0.0 };

__attribute__ ((aligned(256))) struct
{
  volatile uint64_t c1;
  volatile uint64_t c2;
  volatile uint64_t c3;
} counters = { 0, 0, 0 };

/* ---------------------------- local helper functions --------------------- */

static void dump_tdb (struct __htm_tdb *tdb)
{
  unsigned char *p;
  int i;
  int j;

  if (do_dump_tdb == 0)
    {
      return;
    }
  p = (unsigned char *)tdb;
  for (i = 0; i < 16; i++)
    {
      fprintf (stderr, "0x%02x  ", i * 16);
      for (j = 0; j < 16; j++)
	{
	  fprintf (stderr, "%02x", (int)p[i * 16 + j]);
	  if (j < 15)
	    {
	      fprintf (stderr, " ");
	    }
	  if (j == 7)
	    {
	      fprintf (stderr, " ");
	    }
	}
      fprintf (stderr, "\n");
    }

  return;
}

/* ---------------------------- local test functions ----------------------- */

/* Check values of the constants defined in htmintrin.h.  */
static int test_constants (void)
{
  if (_HTM_TBEGIN_STARTED != 0)
    {
      return 100 * _HTM_TBEGIN_STARTED + 1;
    }
  if (_HTM_TBEGIN_INDETERMINATE != 1)
    {
      return 100 * _HTM_TBEGIN_INDETERMINATE + 2;
    }
  if (_HTM_TBEGIN_TRANSIENT != 2)
    {
      return 100 * _HTM_TBEGIN_TRANSIENT + 3;
    }
  if (_HTM_TBEGIN_PERSISTENT != 3)
    {
      return 100 * _HTM_TBEGIN_PERSISTENT + 4;
    }

  return 0;
}

static int test_tbegin_ntstg_tend (void)
{
  int rc;

  counters.c1 = 0;
  counters.c2 = 0;
  if ((rc = __builtin_tbegin ((void *)0)) == 0)
    {
      __builtin_non_tx_store ((uint64_t *)&counters.c1, 1);
      counters.c2 = 2;
      rc = __builtin_tend ();
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

static int test_tbegin_ntstg_tabort (void)
{
  float f;

  counters.c1 = 0;
  counters.c2 = 0;
  f = 0;
  if (__builtin_tbegin ((void *)0) == 0)
    {
      __builtin_non_tx_store ((uint64_t *)&counters.c1, 1);
      counters.c2 = 2;
      f = 1;
      __builtin_tabort (256);
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

static int test_tbegin_nofloat (void)
{
  int rc;

  counters.c1 = 0;
  counters.c2 = 0;
  if ((rc = __builtin_tbegin_nofloat ((void *)0)) == 0)
    {
      __builtin_non_tx_store ((uint64_t *)&counters.c1, 1);
      counters.c2 = 2;
      rc = __builtin_tend ();
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

static int test_tbegin_retry (void)
{
  int rc;

  counters.c1 = 0;
  counters.c2 = 0;
  counters.c3 = 0;
  if ((rc = __builtin_tbegin_retry ((void *)0, 5)) == 0)
    {
      int do_abort;

      do_abort = (counters.c1 == 0) ? 1 : 0;
      __builtin_non_tx_store (
			     (uint64_t *)&counters.c1, counters.c1 + 1);
      if (do_abort == 1)
	{
	  __builtin_tabort (256);
	}
      counters.c2 = counters.c2 + 10;
      __builtin_non_tx_store ((uint64_t *)&counters.c3, 3);
      rc = __builtin_tend ();
      if (rc != 0)
	{
	  return 100 * rc + 5;
	}
      if (counters.c1 != 2)
	{
	  return 100 * counters.c1 + 2;
	}
      if (counters.c2 != 10)
	{
	  return 100 * counters.c2 + 3;
	}
      if (counters.c3 != 3)
	{
	  return 100 * counters.c3 + 6;
	}
    }
  else
    {
      return 100 * rc + 4;
    }

  return 0;
}

static int test_tbegin_retry_nofloat (void)
{
  int rc;

  counters.c1 = 0;
  counters.c2 = 0;
  counters.c3 = 0;
  if ((rc = __builtin_tbegin_retry_nofloat ((void *)0, 5)) == 0)
    {
      int do_abort;

      do_abort = (counters.c1 == 0) ? 1 : 0;
      __builtin_non_tx_store (
			     (uint64_t *)&counters.c1, counters.c1 + 1);
      if (do_abort == 1)
	{
	  __builtin_tabort (256);
	}
      counters.c2 = counters.c2 + 10;
      __builtin_non_tx_store ((uint64_t *)&counters.c3, 3);
      rc = __builtin_tend ();
      if (rc != 0)
	{
	  return 100 * rc + 5;
	}
      if (counters.c1 != 2)
	{
	  return 100 * counters.c1 + 2;
	}
      if (counters.c2 != 10)
	{
	  return 100 * counters.c2 + 3;
	}
      if (counters.c3 != 3)
	{
	  return 100 * counters.c3 + 6;
	}
    }
  else
    {
      return 100 * rc + 4;
    }

  return 0;
}

static int test_tbegin_aborts (void)
{
  float f;
  int rc;

  f = 77;
  if ((rc = __builtin_tbegin ((void *)0)) == 0)
    {
      f = 88;
      __builtin_tabort (256);
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
  if ((rc = __builtin_tbegin ((void *)0)) == 0)
    {
      f = 99;
      __builtin_tabort (257);
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
  if ((rc = __builtin_tbegin ((void *)0)) == 0)
    {
      global.float_3 = global.float_1 + global.float_2;
      rc = __builtin_tend ();
      if (rc != 0)
	{
	  return 100 * rc + 8;
	}
    }
  else
    {
      return 100 * rc + 9;
    }
  if (global.float_3 != global.float_1 + global.float_2)
    {
      return 100 * rc + 10;
    }

  return 0;
}

static __attribute__((noinline)) void indirect_abort(int abort_code)
{
  __builtin_tabort (abort_code);

  return;
}

static int test_tbegin_indirect_aborts (void)
{
  float f;
  int rc;

  f = 77;
  if ((rc = __builtin_tbegin ((void *)0)) == 0)
    {
      f = 88;
      indirect_abort(256);
      return 2;
    }
  else if (rc != 2)
    {
      return 100 * rc + 3;
    }
  if (f != 77)
    {
      return 100 * rc + 4;
    }
  f = 66;
  if ((rc = __builtin_tbegin ((void *)0)) == 0)
    {
      f = 99;
      indirect_abort(257);
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

  return 0;
}

static int test_tbegin_nofloat_aborts (void)
{
  int rc;

  if ((rc = __builtin_tbegin_nofloat ((void *)0)) == 0)
    {
      __builtin_tabort (256);
      return 2;
    }
  if ((rc = __builtin_tbegin_nofloat ((void *)0)) == 0)
    {
      __builtin_tabort (257);
      return 1005;
    }
  else if (rc != 3)
    {
      return 1000 * rc + 6;
    }

  return 0;
}

static int test_tbegin_nofloat_indirect_aborts (void)
{
  int rc;

  if ((rc = __builtin_tbegin_nofloat ((void *)0)) == 0)
    {
      indirect_abort (256);
      return 2;
    }
  if ((rc = __builtin_tbegin_nofloat ((void *)0)) == 0)
    {
      indirect_abort (257);
      return 1005;
    }
  else if (rc != 3)
    {
      return 1000 * rc + 6;
    }

  return 0;
}

static
int _test_tbegin_retry_aborts (int retries, uint64_t abort_code)
{
  int rc;

  counters.c1 = 0;
  if ((rc = __builtin_tbegin_retry ((void *)0, retries)) == 0)
    {
      __builtin_non_tx_store ((uint64_t *)&counters.c1, counters.c1 + 1);
      __builtin_tabort (abort_code);
      return 2;
    }
  else
    {
      if ((abort_code & 1) == 0)
	{
	  if (rc != 2)
	    {
	      return 100 * rc + 2003;
	    }
	  else if (counters.c1 != (uint64_t)retries + 1)
	    {
	      return 1000 * counters.c1 + 100 * retries + 4;
	    }
	}
      else
	{
	  if (rc != 3)
	    {
	      return 100 * rc + 3005;
	    }
	  else if (counters.c1 != 1)
	    {
	      return 1000 * counters.c1 + 100 * retries + 6;
	    }
	}
    }

  return 0;
}

static int test_tbegin_retry_aborts (void)
{
  int rc;
  int retries;

  for (retries = 1; retries <= 3; retries++)
    {
      rc = _test_tbegin_retry_aborts (retries, 256);
      if (rc != 0)
	{
	  return 10000 + rc;
	}
    }
  for (retries = 1; retries <= 3; retries++)
    {
      rc = _test_tbegin_retry_aborts (retries, 257);
      if (rc != 0)
	{
	  return 20000 + rc;
	}
    }
  if ((rc = __builtin_tbegin_retry ((void *)0, 5)) == 0)
    {
      global.float_3 = global.float_1 + global.float_2;
      rc = __builtin_tend ();
      if (rc != 0)
	{
	  return 30000 + 100 * rc + 6;
	}
    }
  else
    {
      return 30000 + 100 * rc + 7;
    }

  return 0;
}

static int _test_tbegin_retry_nofloat_aborts (int retries, uint64_t abort_code)
{
  int rc;

  counters.c1 = 0;
  if ((rc = __builtin_tbegin_retry_nofloat ((void *)0, retries)) == 0)
    {
      __builtin_non_tx_store ((uint64_t *)&counters.c1, counters.c1 + 1);
      __builtin_tabort (abort_code);
      return 2;
    }
  else
    {
      if ((abort_code & 1) == 0)
	{
	  if (rc != 2)
	    {
	      return 100 * rc + 2003;
	    }
	  else if (counters.c1 != (uint64_t)retries + 1)
	    {
	      return 1000 * counters.c1 + 100 * retries + 4;
	    }
	}
      else
	{
	  if (rc != 3)
	    {
	      return 100 * rc + 3005;
	    }
	  else if (counters.c1 != 1)
	    {
	      return 1000 * counters.c1 + 100 * retries + 6;
	    }
	}
    }

  return 0;
}

static int test_tbegin_retry_nofloat_aborts (void)
{
  int rc;
  int retries;

  for (retries = 1; retries <= 3; retries++)
    {
      rc = _test_tbegin_retry_nofloat_aborts (retries, 256);
      if (rc != 0)
	{
	  return 10 * retries + rc;
	}
    }
  for (retries = 1; retries <= 3; retries++)
    {
      rc = _test_tbegin_retry_nofloat_aborts (retries, 257);
      if (rc != 0)
	{
	  return 10000 + 10 * retries + rc;
	}
    }

  return 0;
}

static int test_tbegin_tdb (void)
{
  int rc;

  local_tdb.format = 0;
  if ((rc = __builtin_tbegin (&local_tdb)) == 0)
    {
      rc = __builtin_tend ();
      if (rc != 0)
	{
	  return 100 * rc + 1;
	}
      if (local_tdb.format != 0)
	{
	  dump_tdb (&local_tdb);
	  return 100 * local_tdb.format + 2;
	}
    }
  else
    {
      return 100 * rc + 3;
    }
  local_tdb.format = 0;
  if ((rc = __builtin_tbegin (&local_tdb)) == 0)
    {
      __builtin_tabort (257);
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
	  dump_tdb (&local_tdb);
	  return 100 * local_tdb.format + 6;
	}
    }
  local_tdb256.format = 0;
  if ((rc = __builtin_tbegin (&local_tdb256)) == 0)
    {
      rc = __builtin_tend ();
      if (rc != 0)
	{
	  return 1100 * rc + 1;
	}
      if (local_tdb256.format != 0)
	{
	  dump_tdb (&local_tdb256);
	  return 1100 * local_tdb256.format + 2;
	}
    }
  else
    {
      return 1100 * rc + 3;
    }
  local_tdb256.format = 0;
  if ((rc = __builtin_tbegin (&local_tdb256)) == 0)
    {
      __builtin_tabort (257);
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
	  dump_tdb (&local_tdb256);
	  return 2100 * local_tdb256.format + 6;
	}
    }

  return 0;
}

static int test_tbegin_nofloat_tdb (void)
{
  int rc;

  local_tdb.format = 0;
  if ((rc = __builtin_tbegin_nofloat (&local_tdb)) == 0)
    {
      rc = __builtin_tend ();
      if (rc != 0)
	{
	  return 100 * rc + 1;
	}
      if (local_tdb.format != 0)
	{
	  dump_tdb (&local_tdb);
	  return 100 * local_tdb.format + 2;
	}
    }
  else
    {
      return 3;
    }
  local_tdb.format = 0;
  if ((rc = __builtin_tbegin_nofloat (&local_tdb)) == 0)
    {
      __builtin_tabort (257);
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
	  dump_tdb (&local_tdb);
	  return 100 * local_tdb.format + 6;
	}
    }
  local_tdb256.format = 0;
  if ((rc = __builtin_tbegin_nofloat (&local_tdb256)) == 0)
    {
      rc = __builtin_tend ();
      if (rc != 0)
	{
	  return 1100 * rc + 1;
	}
      if (local_tdb256.format != 0)
	{
	  dump_tdb (&local_tdb256);
	  return 1100 * local_tdb256.format + 2;
	}
    }
  else
    {
      return 1003;
    }
  local_tdb256.format = 0;
  if ((rc = __builtin_tbegin_nofloat (&local_tdb256)) == 0)
    {
      __builtin_tabort (257);
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
	  dump_tdb (&local_tdb256);
	  return 2100 * local_tdb256.format + 6;
	}
    }

  return 0;
}

static int test_tbegin_retry_tdb (void)
{
  int rc;

  local_tdb256.format = 0;
  if ((rc = __builtin_tbegin_retry (&local_tdb256, 2)) == 0)
    {
      rc = __builtin_tend ();
      if (rc != 0)
	{
	  return 1100 * rc + 1;
	}
      if (local_tdb256.format != 0)
	{
	  dump_tdb (&local_tdb256);
	  return 1100 * local_tdb256.format + 2;
	}
    }
  else
    {
      return 1003;
    }
  local_tdb256.format = 0;
  if ((rc = __builtin_tbegin_retry (&local_tdb256, 2)) == 0)
    {
      __builtin_tabort (257);
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
	  dump_tdb (&local_tdb256);
	  return 2100 * local_tdb256.format + 6;
	}
    }

  return 0;
}

static int test_tbegin_retry_nofloat_tdb (void)
{
  int rc;

  local_tdb.format = 0;
  if ((rc = __builtin_tbegin_retry_nofloat (&local_tdb, 2)) == 0)
    {
      rc = __builtin_tend ();
      if (rc != 0)
	{
	  return 100 * rc + 1;
	}
      if (local_tdb.format != 0)
	{
	  dump_tdb (&local_tdb);
	  return 100 * local_tdb.format + 2;
	}
    }
  else
    {
      return 100 * rc + 3;
    }
  local_tdb.format = 0;
  if ((rc = __builtin_tbegin_retry_nofloat (&local_tdb, 2)) == 0)
    {
      __builtin_tabort (257);
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
	  dump_tdb (&local_tdb);
	  return 100 * local_tdb.format + 6;
	}
    }
  local_tdb256.format = 0;
  if ((rc = __builtin_tbegin_retry_nofloat (&local_tdb256, 2)) == 0)
    {
      rc = __builtin_tend ();
      if (rc != 0)
	{
	  return 1100 * rc + 1;
	}
      if (local_tdb256.format != 0)
	{
	  dump_tdb (&local_tdb256);
	  return 1100 * local_tdb256.format + 2;
	}
    }
  else
    {
      return 1100 * rc + 3;
    }
  local_tdb256.format = 0;
  if ((rc = __builtin_tbegin_retry_nofloat (&local_tdb256, 2)) == 0)
    {
      __builtin_tabort (257);
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
	  dump_tdb (&local_tdb256);
	  return 2100 * local_tdb256.format + 6;
	}
    }

  return 0;
}

static int test_etnd (void)
{
  int rc;

  counters.c1 = 0;
  counters.c2 = 0;
  counters.c3 = 0;
  if ((rc = __builtin_tbegin ((void *)0)) == 0)
    {
      counters.c1 = __builtin_tx_nesting_depth ();
      if (__builtin_tbegin ((void *)0) == 0)
	{
	  counters.c2 = __builtin_tx_nesting_depth ();
	  if (__builtin_tbegin ((void *)0) == 0)
	    {
	      counters.c3 = __builtin_tx_nesting_depth ();
	      __builtin_tend ();
	    }
	  __builtin_tend ();
	}
      __builtin_tend ();
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

static int test_tbeginc (void)
{
  int rc;

  counters.c1 = 0;
  __builtin_tbeginc ();
  counters.c1 = 1;
  rc = __builtin_tend ();
  if (rc != 0)
    {
      return 10000 * rc + 1;
    }
  if (counters.c1 != 1)
    {
      return 100000 * counters.c1 + 3;
    }

  return 0;
}

/* ---------------------------- local testing framework functions ---------- */

static int run_one_test (const test_table_entry_t *test_entry)
{
  int do_print_passes;
  int succeeded;
  int rc;
  int i;

  /* Warmup run to get all necessary data and instruction pages into the page
   * tables.  */
  {
    int run;

    do_dump_tdb = 0;
    for (run = 0; run < NUM_WARMUP_RUNS; run++)
      {
	test_entry->test_func ();
      }
    do_dump_tdb = 1;
  }
  do_print_passes = (
		     test_entry->required_quorum != 1 ||
		     test_entry->max_repetitions != 1);
  printf ("RRR RUN  %s\n", test_entry->name);
  if (do_print_passes == 1)
    {
      printf (
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
	      printf ("        ");
	    }
	  else
	    {
	      printf (",");
	    }
	}
      rc = test_entry->test_func ();
      if (rc == 0)
	{
	  if (do_print_passes == 1)
	    {
	      printf (" success");
	    }
	  succeeded++;
	  if (succeeded >= test_entry->required_quorum)
	    {
	      break;
	    }
	}
      else
	{
	  printf (" failed (rc = %d)", rc);
	}
    }
  if (do_print_passes == 1 || rc != 0)
    {
      printf ("\n");
    }
  if (succeeded >= test_entry->required_quorum)
    {
      printf ("+++ OK   %s\n", test_entry->name);

      return 0;
    }
  else
    {
      printf ("--- FAIL %s\n", test_entry->name);

      return (rc != 0) ? rc : -1;
    }
}

static int run_all_tests (const test_table_entry_t *test_table)
{
  const test_table_entry_t *test;
  int rc;

  for (
       rc = 0, test = &test_table[0];
       test->test_func != NULL && rc == 0; test++)
    {
      rc = run_one_test (test);
    }

  return rc;
}

/* ---------------------------- interface functions ------------------------ */

int main (void)
{
  const test_table_entry_t test_table[] = {
    TEST_NO_REP (test_constants),
    TEST_DF_REP (test_tbegin_ntstg_tend),
    TEST_DF_REP (test_tbegin_ntstg_tabort),
    TEST_DF_REP (test_tbegin_nofloat),
    TEST_NO_REP (test_tbegin_retry),
    TEST_NO_REP (test_tbegin_retry_nofloat),
    TEST_DF_REP (test_tbegin_aborts),
    TEST_DF_REP (test_tbegin_indirect_aborts),
    TEST_DF_REP (test_tbegin_nofloat_aborts),
    TEST_DF_REP (test_tbegin_nofloat_indirect_aborts),
    TEST_NO_REP (test_tbegin_retry_aborts),
    TEST_NO_REP (test_tbegin_retry_nofloat_aborts),
    TEST_DF_REP (test_tbegin_tdb),
    TEST_DF_REP (test_tbegin_nofloat_tdb),
    TEST_NO_REP (test_tbegin_retry_tdb),
    TEST_NO_REP (test_tbegin_retry_nofloat_tdb),
    TEST_DF_REP (test_etnd),
    TEST_DF_REP (test_tbeginc),
    { (void *)0, 0, 0 }
  };

  {
    int rc;

    rc = run_all_tests (test_table);

    return rc;
  }
}
