/* { dg-skip-if "requires hosted libstdc++ for stdlib malloc" { ! hostedlib } } */

/* A toy re-implementation of CPython's object model.  */

#include <stdlib.h>
#include <string.h>

#include "../../gcc.dg/analyzer/analyzer-decls.h"

typedef struct base_obj base_obj;
typedef struct string_obj string_obj;

struct base_obj
{
  int ob_refcnt;
};

struct string_obj
{
  base_obj str_base;
  size_t str_len;
  char str_buf[];
};

base_obj *alloc_obj (const char *str)
{
  size_t len = strlen (str);
  base_obj *obj = (base_obj *)malloc (sizeof (string_obj) + len + 1);
  if (!obj)
    return NULL;
  obj->ob_refcnt = 1;
  string_obj *str_obj = (string_obj *)obj;
  __analyzer_eval (str_obj->str_base.ob_refcnt == 1); /* { dg-warning "TRUE" } */
  return obj;
}
