#include "../../gcc.dg/analyzer/analyzer-decls.h"
typedef __SIZE_TYPE__ size_t;

extern void *calloc (size_t __nmemb, size_t __size)
  __attribute__ ((__nothrow__ , __leaf__))
  __attribute__ ((__malloc__))
  __attribute__ ((__alloc_size__ (1, 2))) ;

char *test_1 (size_t sz)
{
  char *p;

  p = (char *) calloc (1, 3);
  if (!p)
    return NULL;

  __analyzer_dump_capacity (p); /* { dg-warning "capacity: '\\(\[^\n\r\]*\\)3'" } */

  __analyzer_eval (p[0] == 0); /* { dg-warning "TRUE" } */
  __analyzer_eval (p[1] == 0); /* { dg-warning "TRUE" } */
  __analyzer_eval (p[2] == 0); /* { dg-warning "TRUE" } */

  return p;
}

char **
test_pr113333_1 (void)
{
  char **p = (char **)calloc (1, sizeof(char *));
  if (p)
    {
      __analyzer_eval (*p == 0); /* { dg-warning "TRUE" } */
      __analyzer_eval (p[0] == 0); /* { dg-warning "TRUE" } */
    }
  return p;
}

char **
test_pr113333_2 (void)
{
  char **p = (char **)calloc (2, sizeof(char *));
  if (p)
    {
      __analyzer_eval (*p == 0); /* { dg-warning "TRUE" } */
      __analyzer_eval (p[0] == 0); /* { dg-warning "TRUE" } */
      __analyzer_eval (p[1] == 0); /* { dg-warning "TRUE" } */
    }
  return p;
}

char **
test_pr113333_3 (void)
{
  char **vec = (char **)calloc (1, sizeof(char *));
  if (vec)
    for (char **p=vec ; *p ; p++); /* { dg-bogus "heap-based buffer over-read" } */
  return vec;
}
