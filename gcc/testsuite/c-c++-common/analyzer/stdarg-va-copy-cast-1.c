/* Verify that we don't ICE in get_va_copy_arg when the pointer to a
   va_list was obtained from a cast of a non-pointer value (for targets
   where va_list is an array type).  */

void
test_intptr (__INTPTR_TYPE__ i)
{
  __builtin_va_list *p = (__builtin_va_list *) i;
  __builtin_va_list ap;
  __builtin_va_copy (ap, *p);
  __builtin_va_end (ap);
}

void
test_int (int i)
{
  __builtin_va_list *p = (__builtin_va_list *) (__INTPTR_TYPE__) i;
  __builtin_va_list ap;
  __builtin_va_copy (ap, *p);
  __builtin_va_end (ap);
}
