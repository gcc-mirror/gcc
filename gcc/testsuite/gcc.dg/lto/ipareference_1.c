static int val;
int set_val (void)
{
  val = 5;
}
int get_val (void)
{
  return val;
}
__attribute__ ((__noinline__)) void
do_nothing ()
{
  asm volatile ("":::"memory");
}
