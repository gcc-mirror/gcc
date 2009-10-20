/* { dg-lto-do assemble } */

typedef __SIZE_TYPE__ size_t;
static int stack_dir;
static void find_stack_direction ()
{
  static char *addr = ((void *)0);
  auto char dummy;
  if (addr == ((void *)0))
    {
      addr = &(dummy);
      find_stack_direction ();
    }
}
void * C_alloca (size_t size)
{
  if (stack_dir == 0)
    find_stack_direction ();
}

