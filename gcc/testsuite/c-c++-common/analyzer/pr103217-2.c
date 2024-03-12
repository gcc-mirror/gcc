/* { dg-additional-options "-Wno-analyzer-too-complex" } */

typedef __SIZE_TYPE__ size_t;

extern void *calloc (size_t __nmemb, size_t __size)
  __attribute__ ((__nothrow__ , __leaf__, __malloc__, __alloc_size__ (1, 2)));

extern char *strdup (const char *__s)
  __attribute__ ((__nothrow__ , __leaf__, __malloc__, __nonnull__ (1)));

extern void abort (void)
  __attribute__ ((__nothrow__ , __leaf__, __noreturn__));

extern int getopt (int ___argc, char *const *___argv, const char *__shortopts)
  __attribute__ ((__nothrow__ , __leaf__, __nonnull__ (2, 3)));
extern char *optarg;

extern void free (void *__ptr)
  __attribute__ ((__nothrow__ , __leaf__));

char *xstrdup(const char *src) {
	char *val = strdup(src);
	if (!val)
		abort();
	return val;
}

struct test {
	char *one, *two;
};

int main(int argc, char *argv[]) {
	struct test *options = (struct test *) calloc(1, sizeof(*options));
	int rc;
	if (!options)
		abort();

	while ((rc = getopt(argc, argv, "a:b:")) != -1) {
		switch (rc) {
		case 'a':
			free(options->one);
			options->one = xstrdup(optarg);
			break;
		case 'b':
			free(options->two);
			options->two = xstrdup(optarg);
			break;
		}
	}
	free(options->one);
	free(options->two);
	free(options);
	return 0;
}
