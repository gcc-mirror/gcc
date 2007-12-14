typedef __builtin_va_list va_list;
void gftp_config_parse_args (int numargs, char **first, ...)
{
  char **dest = first;
  va_list argp;
  __builtin_va_start (argp, first);
  while (numargs-- > 0)
    {
      *dest = __builtin_malloc (1);
      dest = __builtin_va_arg(argp, char **);
      *dest = ((void *)0);
    }
  __builtin_va_end(argp);
}

