typedef __SIZE_TYPE__ size_t;

int getrandom (void *__buffer, size_t __length, /* { dg-line getrandom } */
	       unsigned int __flags)
  __attribute__ ((access (__write_only__, 1, 2)));

/* { dg-message "parameter 1 of 'getrandom' marked with attribute 'access \\(write_only, 1, 2\\)'" "" { target c} getrandom } */
/* { dg-message "parameter 1 of 'int getrandom\\(void\\*, size_t, unsigned int\\)' marked with attribute 'access \\(write_only, 1, 2\\)'" "" { target c++ } getrandom } */

#define GRND_RANDOM 0x02

void test (int flag)
{
  char *buf;

  if (flag)
    buf = (char *) __builtin_malloc (1024);
  else
    buf = (char *)""; /* { dg-message "here" } */

  if (getrandom(buf, 16, GRND_RANDOM)) /* { dg-warning "write to string literal" } */
    __builtin_printf("%s\n", buf);

  if (flag)
    __builtin_free (buf);
}
