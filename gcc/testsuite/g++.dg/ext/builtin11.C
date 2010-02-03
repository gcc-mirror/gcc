// PR c++/40138
// { dg-options "-Wall" }

void foo(int i, ...)
{
  V v;				// { dg-error "not declared|expected" }
  __builtin_va_start(v, i);	// { dg-error "not declared" }
  i = __builtin_va_arg(v, int);
}
