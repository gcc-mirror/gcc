struct S {
  struct { } empty[1];
  int i;
};

int foo(int i, ...)
{
  struct S s;
  __builtin_va_list va;
  __builtin_va_start(va, i);
  s = __builtin_va_arg(va, struct S);
  __builtin_va_end(va);
  return s.i;
}
