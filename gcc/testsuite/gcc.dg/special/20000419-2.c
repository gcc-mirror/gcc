/* A static function with a global alias should not get 'defined but
   not used' warnings.  Exposed by Linux kernel.  */
/* { dg-do compile } */
/* { dg-require-alias "" } */
/* { dg-options "-Wall" } */

extern void do_something (void);
extern void do_something_else (void);

static int
init_foobar(void)  /* { dg-bogus "defined but not used" "not used warning" } */
{
  do_something();
  do_something_else();
  return 0;
}

int init_module(void) __attribute__((alias("init_foobar")));
