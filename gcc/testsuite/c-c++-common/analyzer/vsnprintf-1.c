typedef __SIZE_TYPE__ size_t;

int
my_snprintf(char *pos, size_t left, const char *fmt, ...)
{
  __builtin_va_list ap;
  __builtin_va_start(ap, fmt);
  const int len = __builtin_vsnprintf(pos, left, fmt, ap);
  __builtin_va_end(ap);
  return len;
}
