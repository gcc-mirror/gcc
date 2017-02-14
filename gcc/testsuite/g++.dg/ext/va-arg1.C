// PR c++/33462

struct A {};

void foo()
{
  ++__builtin_va_arg (0, A);
  // { dg-error "operand type is 'A'" "" {target *-*-*} "7" }
  // { dg-error "first argument to 'va_arg' not of type 'va_list'" "" {target *-*-*} "7" }
}
