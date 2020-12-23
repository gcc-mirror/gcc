// { dg-additional-options -fmodules-ts }

module foo;

void frob ()
{
  __throw_with_nested_impl (integral_constant<bool, true> ());
}
