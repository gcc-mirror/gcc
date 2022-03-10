typedef __SIZE_TYPE__ size_t;

int getrandom (void *__buffer, size_t __length,
	       unsigned int __flags)
  __attribute__ ((access (__write_only__, 1, 2)));

#define GRND_RANDOM 0x02

const char *test = "test";

int main(void)
{
	const char buf[5] = { 0 };

	if (getrandom((char *)test, sizeof(buf), GRND_RANDOM)) /* { dg-warning "write to string literal" } */
		__builtin_printf("%s\n", buf);

	return 0;
}
