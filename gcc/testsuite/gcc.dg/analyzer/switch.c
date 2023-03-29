/* { dg-additional-options "-fanalyzer-transitivity" } */

#include "analyzer-decls.h"

void test (int i)
{
  switch (i)
    {
    case 0:
      __analyzer_eval (i == 0); /* { dg-warning "TRUE" } */
      __analyzer_eval (i != -1); /* { dg-warning "TRUE" } */
      __analyzer_eval (i != 0); /* { dg-warning "FALSE" } */
      __analyzer_eval (i != 1); /* { dg-warning "TRUE" } */
      break;

    case 3 ... 5:
      __analyzer_eval (i != 0); /* { dg-warning "TRUE" } */
      __analyzer_eval (i > 1); /* { dg-warning "TRUE" } */
      __analyzer_eval (i > 2); /* { dg-warning "TRUE" } */
      __analyzer_eval (i >= 2); /* { dg-warning "TRUE" } */
      __analyzer_eval (i >= 3); /* { dg-warning "TRUE" } */
      __analyzer_eval (i <= 5); /* { dg-warning "TRUE" } */
      __analyzer_eval (i < 6); /* { dg-warning "TRUE" } */
      __analyzer_eval (i <= 6); /* { dg-warning "TRUE" } */
      __analyzer_eval (i < 7); /* { dg-warning "TRUE" } */
      __analyzer_eval (i != 6); /* { dg-warning "TRUE" } */
      __analyzer_eval (i != 3); /* { dg-warning "UNKNOWN" } */
      __analyzer_eval (i != 4); /* { dg-warning "UNKNOWN" } */
      __analyzer_eval (i != 5); /* { dg-warning "UNKNOWN" } */
      __analyzer_eval (i >= 4); /* { dg-warning "UNKNOWN" } */
      __analyzer_eval (i >= 5); /* { dg-warning "UNKNOWN" } */
      __analyzer_eval (i <= 3); /* { dg-warning "UNKNOWN" } */
      __analyzer_eval (i <= 4); /* { dg-warning "UNKNOWN" } */
      break;

    default:
      __analyzer_eval (i == -1); /* { dg-warning "UNKNOWN" } */
      __analyzer_eval (i == 0); /* { dg-warning "FALSE" } */
      __analyzer_eval (i == 2); /* { dg-warning "UNKNOWN" } */
      __analyzer_eval (i == 3); /* { dg-warning "FALSE" } */
      __analyzer_eval (i == 4); /* { dg-warning "FALSE" } */
      __analyzer_eval (i == 5); /* { dg-warning "FALSE" } */
      __analyzer_eval (i == 6); /* { dg-warning "UNKNOWN" } */
      __analyzer_eval (i != 0); /* { dg-warning "TRUE" } */
      __analyzer_eval (i != 1); /* { dg-warning "UNKNOWN" } */
      __analyzer_eval (i != 3); /* { dg-warning "TRUE" } */
      __analyzer_eval (i != 4); /* { dg-warning "TRUE" } */
      __analyzer_eval (i != 5); /* { dg-warning "TRUE" } */
      __analyzer_eval (i != 6); /* { dg-warning "UNKNOWN" } */
      break;
    }
}

/* Verify that the analyzer follows the correct paths on a
   switch statement guarded by an if, using noinline to defeat
   optimizations.  */

static void __attribute__((noinline))
__analyzer_called_by_test_2 (int y)
{
  switch (y)
    {
    case 0:
      __analyzer_dump_path (); /* { dg-bogus "path" } */
      break;
    case 1:
      __analyzer_dump_path (); /* { dg-message "path" } */
      break;
    case 2:
      __analyzer_dump_path (); /* { dg-bogus "path" } */
      break;
    default:
      __analyzer_dump_path (); /* { dg-bogus "path" } */
      break;
    }
}

void test_2 (int x)
{
  if (x == 1)
    __analyzer_called_by_test_2 (x);
}

void test_3 (int x, int y)
{
  if (y == 3)
    switch (x)
      {
      case 0 ... 9:
      case 20 ... 29:
	if (x == y)
	  __analyzer_dump_path (); /* { dg-message "path" } */
	else
	  __analyzer_dump_path (); /* { dg-message "path" } */
      }
}

struct s4
{
  unsigned char level:3;
  unsigned char key_id_mode:2;
  unsigned char reserved:3;
};

void test_4 (struct s4 *p)
{
  switch (p->key_id_mode)
    {
    case 0:
      __analyzer_dump_path (); /* { dg-message "path" } */
      break;
    case 1:
      __analyzer_dump_path (); /* { dg-message "path" } */
      break;
    case 2:
      __analyzer_dump_path (); /* { dg-message "path" } */
      break;
    case 3:
      __analyzer_dump_path (); /* { dg-message "path" } */
      break;
    }
  __analyzer_dump_path (); /* { dg-message "path" } */
}

int test_5 (unsigned v)
{
  switch (v)
    {
    case 0:
      return 7;
      break;
    case 1:
      return 23;
      break;
    default:
      return v * 2;
    }
}

int test_6 (unsigned v)
{
  switch (v)
    {
    case 0:
      return 3;
    case -1:
      return 22;
    }
  return -3;
}

int g7 = -1;
int test_7 ()
{
	switch (g7++) {
	case 0:
	  return 32;

	case 100:
	  return 42;
	}
	return 0;
}

int test_bitmask_1 (int x)
{
  int flag = 0;
  if (x & 0x80)
    flag = 1;

  switch (x)
    {
    case 0:
      if (flag)
	__analyzer_dump_path ();  /* { dg-bogus "path" } */
      else
	__analyzer_dump_path ();  /* { dg-message "path" } */
      break;

    case 0x80:
      if (flag)
	__analyzer_dump_path ();  /* { dg-message "path" } */
      else
	__analyzer_dump_path ();  /* { dg-bogus "path" } */
      break;

    case 0x81:
      if (flag)
	__analyzer_dump_path ();  /* { dg-message "path" } */
      else
	__analyzer_dump_path ();  /* { dg-bogus "path" } */
      break;
    }
}

int test_bitmask_2 (int x)
{
  int flag = 0;
  if ((x & 0xf80) == 0x80)
    flag = 1;

  switch (x)
    {
    case 0:
      if (flag)
	__analyzer_dump_path ();  /* { dg-bogus "path" } */
      else
	__analyzer_dump_path ();  /* { dg-message "path" } */
      break;

    case 0x80:
      if (flag)
	__analyzer_dump_path ();  /* { dg-message "path" } */
      else
	__analyzer_dump_path ();  /* { dg-bogus "path" } */
      break;

    case 0x81:
      if (flag)
	__analyzer_dump_path ();  /* { dg-message "path" } */
      else
	__analyzer_dump_path ();  /* { dg-bogus "path" } */
      break;

    case 0x180:
      if (flag)
	__analyzer_dump_path ();  /* { dg-bogus "path" } */
      else
	__analyzer_dump_path ();  /* { dg-message "path" } */
      break;

    case 0xf80:
      if (flag)
	__analyzer_dump_path ();  /* { dg-bogus "path" } */
      else
	__analyzer_dump_path ();  /* { dg-message "path" } */
      break;
    }
}
