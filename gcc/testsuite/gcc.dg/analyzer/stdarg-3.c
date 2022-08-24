typedef __builtin_va_list va_list;

struct printf_spec {
  unsigned int type;
};

int
format_decode(const char *fmt, struct printf_spec *spec);

static int vbin_printf(const char *fmt, va_list args) {
  struct printf_spec spec;
  int width = 0;

  while (*fmt) {
    int read = format_decode(fmt, &spec);

    fmt += read;

    switch (spec.type) {
    case 0:
      break;
    case 1:
      width = __builtin_va_arg(args, int); /* { dg-bogus "-Wanalyzer-va-list-exhausted" } */
      break;
    }
  }

  return width;
}

int bprintf(const char *fmt, ...) {
  va_list args;
  int ret;

  __builtin_va_start(args, fmt);
  ret = vbin_printf(fmt, args);
  __builtin_va_end(args);

  return ret;
}

static int called_by_test_2 (va_list args)
{
  return __builtin_va_arg(args, int); /* { dg-bogus "-Wanalyzer-va-list-exhausted" } */
}

int test_2 (const char *fmt, ...)
{
  va_list args;
  int ret;

  __builtin_va_start (args, fmt);
  ret = called_by_test_2 (args);
  __builtin_va_end (args);

  return ret;
}
