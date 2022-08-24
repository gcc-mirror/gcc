/* { dg-skip-if "" { *-*-* } { "-fno-fat-lto-objects" } { "" } } */

#include "../analyzer-decls.h"

/* va_arg in loop, with no caller to function containing va_start.  */

int test_1a (const char *fmt, ...)
{
  __builtin_va_list args;
  int sum = 0;
  char ch;

  __builtin_va_start(args, fmt);

  while (ch = *fmt++)
    if (ch == '%')
      sum += __builtin_va_arg(args, int);

  __builtin_va_end(args);

  return sum;
}

/* va_arg in loop, with no caller to function containing va_start.  */

static int test_1b_callee (const char *fmt, __builtin_va_list args)
{
  int sum = 0;
  char ch;

  while (ch = *fmt++)
    if (ch == '%')
      sum += __builtin_va_arg(args, int);

  return sum;
}

int test_1b_caller (const char *fmt, ...)
{
  __builtin_va_list args;
  int sum = 0;

  __builtin_va_start(args, fmt);

  sum = test_1b_callee (fmt, args);

  __builtin_va_end(args);

  return sum;
}

/* va_arg in loop, with a caller to the function containing va_start,
   with specific args.  */

static int
test_1c_inner (const char *fmt, __builtin_va_list args)
{
  int sum = 0;
  char ch;

  while (ch = *fmt++)
    if (ch == '%')
      sum += __builtin_va_arg(args, int);

  return sum;
}

static int
test_1c_middle (const char *fmt, ...)
{
  __builtin_va_list args;
  int sum = 0;

  __builtin_va_start(args, fmt);

  sum = test_1c_inner (fmt, args);

  __builtin_va_end(args);

  return sum;
}

void test_1c_outer (void)
{
  int sum = test_1c_middle ("%%", 42, 17);

  __analyzer_describe (0, sum); /* { dg-message "'\\(int\\)59'" } */
}

/* va_arg in loop, with no caller to function containing va_start.  */

int test_2a (int count, ...)
{
  __builtin_va_list args;
  int sum = 0;
  char ch;

  __builtin_va_start(args, count);

  while (count-- > 0)
    sum += __builtin_va_arg(args, int);

  __builtin_va_end(args);

  return sum;
}

/* va_arg in loop, with no caller to function containing va_start.  */

static int test_2b_callee (int count, __builtin_va_list args)
{
  int sum = 0;

  while (count-- > 0)
    sum += __builtin_va_arg(args, int);

  return sum;
}

int test_2b_caller (int count, ...)
{
  __builtin_va_list args;
  int sum = 0;

  __builtin_va_start(args, count);

  sum = test_2b_callee (count, args);

  __builtin_va_end(args);

  return sum;
}

/* va_arg in loop, with a caller to the function containing va_start,
   with specific args.  */

static int test_2c_inner (int count, __builtin_va_list args)
{
  int sum = 0;

  while (count-- > 0)
    sum += __builtin_va_arg(args, int);

  return sum;
}

int test_2c_middle (int count, ...)
{
  __builtin_va_list args;
  int sum = 0;

  __builtin_va_start(args, count);

  sum = test_2c_inner (count, args);

  __builtin_va_end(args);

  return sum;
}

void test_2c_outer (void)
{
  int sum = test_2c_middle (2, 50, 42);

  __analyzer_describe (0, sum); /* { dg-message "'\\(int\\)92'" } */
}

/* va_arg in loop, with no caller to function containing va_start.  */

int test_3a (int placeholder, ...)
{
  __builtin_va_list args;
  int sum = 0;
  int val;

  __builtin_va_start(args, placeholder);

  while (val = __builtin_va_arg(args, int))
    sum += val;

  __builtin_va_end(args);

  return sum;
}

/* va_arg in loop, with no caller to function containing va_start.  */

static int test_3b_callee (__builtin_va_list args)
{
  int sum = 0;
  int val;
  while (val = __builtin_va_arg(args, int))
    sum += val;
  return sum;
}

int test_3b_caller (int placeholder, ...)
{
  __builtin_va_list args;
  int sum = 0;

  __builtin_va_start(args, placeholder);

  sum = test_3b_callee (args);

  __builtin_va_end(args);

  return sum;
}

/* va_arg in loop, with a caller to the function containing va_start,
   with specific args.  */

static int test_3c_inner (__builtin_va_list args)
{
  int sum = 0;
  int val;
  while (val = __builtin_va_arg(args, int))
    sum += val;
  return sum;
}

int test_3c_middle (int placeholder, ...)
{
  __builtin_va_list args;
  int sum = 0;

  __builtin_va_start(args, placeholder);

  sum = test_3c_inner (args);

  __builtin_va_end(args);

  return sum;
}

void test_3c_outer (void)
{
  int sum = test_3c_middle (0, 5, 12, 0);
  __analyzer_describe (0, sum); /* { dg-message "'\\(int\\)17'" } */
}

/* va_arg in loop, with no caller to function containing va_start,
   with a va_copy.  */

static int test_3d_callee (__builtin_va_list args)
{
  int sum = 0;
  int val;
  while (val = __builtin_va_arg(args, int))
    sum += val;
  return sum;
}

int test_3d_caller (int placeholder, ...)
{
  __builtin_va_list args1, args2;
  int sum = 0;

  __builtin_va_start(args1, placeholder);
  __builtin_va_copy (args2, args1);

  sum = test_3d_callee (args1);
  __builtin_va_end(args1);

  sum += test_3d_callee (args2);
  __builtin_va_end(args2);

  return sum;
}

/* va_arg in loop, with a caller to the function containing va_start,
   with specific args, with a va_copy.  */

static int test_3e_inner (__builtin_va_list args)
{
  int sum = 0;
  int val;
  while (val = __builtin_va_arg(args, int))
    sum += val;
  return sum;
}

int test_3e_middle (int placeholder, ...)
{
  __builtin_va_list args1, args2;
  int sum = 0;

  __builtin_va_start(args1, placeholder);
  __builtin_va_copy (args2, args1);

  sum = test_3e_inner (args1);
  __builtin_va_end(args1);

  sum += test_3e_inner (args2);
  __builtin_va_end(args2);

  return sum;
}

void test_3e_outer (void)
{
  int sum = test_3e_middle (0, 5, 6, 0);
  __analyzer_describe (0, sum); /* { dg-message "'\\(int\\)22'" } */
}

/* va_arg in loop, with specific symbolic args.  */

static int test_3f_callee (int placeholder, ...)
{
  __builtin_va_list args;
  int sum = 0;
  int val;

  __builtin_va_start(args, placeholder);

  while (val = __builtin_va_arg(args, int))
    sum += val;

  __builtin_va_end(args);

  return sum;
}

void test_3f_caller (int x, int y, int z)
{
  int sum = test_3f_callee (0, x, y, z, 0);
  __analyzer_describe (0, sum); /* { dg-message "'UNKNOWN\\(int\\)'" } */
}
