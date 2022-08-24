/* Verify that we can disable warnings that have notes added to them via
   region_model_context_decorator::add_note.  */

/* { dg-additional-options "-Wno-analyzer-write-to-string-literal" } */

typedef __SIZE_TYPE__ size_t;

int getrandom (void *__buffer, size_t __length, /* { dg-bogus "parameter 1 of 'getrandom' marked with attribute 'access \\(write_only, 1, 2\\)'" } */
	       unsigned int __flags)
  __attribute__ ((access (__write_only__, 1, 2)));

#define GRND_RANDOM 0x02

void test (int flag)
{
  char *buf;

  if (flag)
    buf = __builtin_malloc (1024);
  else
    buf = (char *)""; /* { dg-bogus "here" } */

  if (getrandom(buf, 16, GRND_RANDOM)) /* { dg-bogus "write to string literal" } */
    __builtin_printf("%s\n", buf);

  if (flag)
    __builtin_free (buf);
}
