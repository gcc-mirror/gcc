typedef long unsigned int size_t;
extern void abort (void);
extern char *strcpy (char *, const char *);
extern int strcmp (const char *, const char *);
typedef __builtin_va_list va_list;
static const char null[] = "(null)";
int g (char *s, const char *format, va_list ap)
{
  const char *f;
  const char *string;
  char spec;
  static const void *step0_jumps[] = {
    &&do_precision,
    &&do_form_integer,
    &&do_form_string,
  };
  f = format;
  if (*f == '\0')
    goto all_done;
  do
    {
      spec = (*++f);
      goto *(step0_jumps[2]);
      
    /* begin switch table. */
    do_precision:
      ++f;
      __builtin_va_arg (ap, int);
      spec = *f;
      goto *(step0_jumps[2]);
      
      do_form_integer:
	__builtin_va_arg (ap, unsigned long int);
	goto end;
	
      do_form_string:
	string = __builtin_va_arg (ap, const char *);
	strcpy (s, string);
      
      /* End of switch table. */
      end:
      ++f;
    }
  while (*f != '\0');

all_done:
  return 0;
}

void
f (char *s, const char *f, ...)
{
  va_list ap;
  __builtin_va_start (ap, f);
  g (s, f, ap);
  __builtin_va_end (ap);
}

int
main (void)
{
  char buf[10];
  f (buf, "%s", "asdf", 0);
  if (strcmp (buf, "asdf"))
    abort ();
  return 0;
}

