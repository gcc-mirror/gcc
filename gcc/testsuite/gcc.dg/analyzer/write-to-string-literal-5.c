/* Verify that deduplication of -Wanalyzer-write-to-string-literal (and their
   notes) works.  */

/* { dg-additional-options "-fanalyzer-show-duplicate-count" } */
/* { dg-require-effective-target alloca } */

#include "analyzer-decls.h"

typedef __SIZE_TYPE__ size_t;

int getrandom (void *__buffer, size_t __length, /* { dg-message "parameter 1 of 'getrandom' marked with attribute 'access \\(write_only, 1, 2\\)'" } */
	       unsigned int __flags)
  __attribute__ ((access (__write_only__, 1, 2)));

#define GRND_RANDOM 0x02

void *test (int flag)
{
  char *ptr;
  if (flag)
    ptr = __builtin_malloc (1024);
  else
    ptr = __builtin_alloca (1024);

  __analyzer_dump_exploded_nodes (0); /* { dg-warning "2 processed enodes" } */

  if (getrandom((char *)"foo", 3, GRND_RANDOM)) /* { dg-warning "write to string literal" "warning" } */
    /* { dg-message "1 duplicate" "dup" { target *-*-* } .-1 } */
    __builtin_printf("ok\n");

  return ptr;
}
