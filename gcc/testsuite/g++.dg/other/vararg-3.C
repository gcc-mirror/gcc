// PR c++/31488: va_list considered non-POD on alpha
// { dg-do compile }

typedef __builtin_va_list __gnuc_va_list;
typedef __gnuc_va_list va_list;

extern int foo (int a, int b, ...);

int bar (int a, int b, ...)
{
  va_list args;
  __builtin_va_start(args,b);
  int result = foo (a, b, args);
  __builtin_va_end(args);
  return result;
}
