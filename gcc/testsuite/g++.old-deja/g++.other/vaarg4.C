// { dg-do assemble  }

// Bug 845. We were treating __builtin_va_arg as a unary expr, not a primary,
// and hence getting parse errors.

typedef __builtin_va_list __gnuc_va_list;
typedef __gnuc_va_list va_list;

struct X { int y; };

void func(va_list va)
{
  char* a = __builtin_va_arg(va, char**)[0];
  int b = __builtin_va_arg(va, X*)->y;
}
