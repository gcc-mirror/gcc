/* { dg-require-effective-target indirect_calls } */

void fn_show_state(void);
typedef void (*fn_handler_fn)(void);
static fn_handler_fn fn_handler[1];

void k_spec(unsigned char value)
{
  void *func = fn_handler[value];
  if (func == fn_show_state )
    return;
  fn_handler[value]();
}
