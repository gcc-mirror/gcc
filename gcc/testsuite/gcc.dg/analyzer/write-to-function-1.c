/* { C only: C++ does not allow for conversion from function pointer to 'void *' */

typedef __SIZE_TYPE__ size_t;

int getrandom (void *__buffer, size_t __length, /* { dg-message "parameter 1 of 'getrandom' marked with attribute 'access \\(write_only, 1, 2\\)'" } */
	       unsigned int __flags)
  __attribute__ ((access (__write_only__, 1, 2)));

#define GRND_RANDOM 0x02

void test (void)
{
  char buf[16];

  if (getrandom(test, 16, GRND_RANDOM)) /* { dg-warning "write to function 'test'" } */
    __builtin_printf("%s\n", buf);
}
