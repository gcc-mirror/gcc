/* { dg-do compile } */
/* { dg-options "-O2 -g" } */

/* Check if gcc splits a call from its CALL_ARG_LOCATION note.  If so,
   we get an ICE in dwarf2out_var_location.  */

typedef void Trans_NS_std_new_handler();
void *operator new(unsigned)
{
  void *p;
  while (__builtin_expect(p == 0, false))
    {
      Trans_NS_std_new_handler handler;
      try {
	handler();
      } catch (int) {
      }
    }
  return (void*) 0xdead;
}
