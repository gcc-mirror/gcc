// PR c++/33462

struct A {};

void foo()
{
  ++__builtin_va_arg(0, A); // { dg-error "'\\+\\+va_arg\\(0, A\\)'" }
}
