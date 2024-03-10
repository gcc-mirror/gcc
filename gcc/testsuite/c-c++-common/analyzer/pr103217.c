/* { dg-additional-options "-Wno-analyzer-too-complex" } */

extern char *strdup (const char *__s)
  __attribute__ ((__nothrow__ , __leaf__, __malloc__, __nonnull__ (1)));

extern void abort (void)
  __attribute__ ((__nothrow__ , __leaf__, __noreturn__));

extern int getopt (int ___argc, char *const *___argv, const char *__shortopts)
  __attribute__ ((__nothrow__ , __leaf__, __nonnull__ (2, 3)));
extern char *optarg;

extern void free (void *__ptr)
  __attribute__ ((__nothrow__ , __leaf__));

#include "../../gcc.dg/analyzer/analyzer-decls.h"

char *xstrdup(const char *src) {
	char *val = strdup(src);
	if (!val)
		abort();
	return val;
}

int main(int argc, char *argv[]) {
	char *one = NULL, *two = NULL;
	int rc;

	while ((rc = getopt(argc, argv, "a:b:")) != -1) {
		switch (rc) {
		case 'a':
			free(one);
			one = xstrdup(optarg);
			break;
		case 'b':
			free(two);
			two = xstrdup(optarg);
			break;
		}
	}
	free(one);
	free(two);
	return 0;
}
