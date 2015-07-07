/* Check that a change mode to a static function is correctly handled. */
/* { dg-do run } */

static void
 __attribute__((__noinline__)) 
foo (void)
{
  __asm__ ("");
}

static void
__attribute__((__noinline__)) 
__attribute__((target("thumb")))
bar (void)
{
  __asm__ ("");
}

int
main (void)
{
  foo();
  bar();
  return 0;
}
