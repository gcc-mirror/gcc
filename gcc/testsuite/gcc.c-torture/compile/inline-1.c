typedef __builtin_va_list va_list;

extern void foo (va_list);

static void
build_message_string (const char *msg, ...)
{
  va_list ap;

  __builtin_va_start (ap, msg);
  foo (ap);
  __builtin_va_end (ap);
}

void
file_name_as_prefix (f)
     const char *f;
{
  build_message_string ("%s: ", f);
}

