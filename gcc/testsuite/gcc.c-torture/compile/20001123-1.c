
typedef __builtin_va_list __gnuc_va_list;
typedef __gnuc_va_list va_list;

struct X { int y; };

void func(va_list va)
{
  char* a = __builtin_va_arg(va, char**)[0];
  int b = __builtin_va_arg(va, struct X*)->y;
}
