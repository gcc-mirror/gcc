/* A toy re-implementation of CPython's object model.  */

#include <stddef.h>
#include <string.h>
#include <stdlib.h>
#include "analyzer-decls.h"

typedef struct base_obj base_obj;
typedef struct type_obj type_obj;
typedef struct string_obj string_obj;

struct base_obj
{
  struct type_obj *ob_type;
  int ob_refcnt;
};

struct type_obj
{
  base_obj tp_base;
};

struct string_obj
{
  base_obj str_base;
  size_t str_len;
  char str_buf[];
};

type_obj type_type = {
  { &type_type, 1},
};

type_obj str_type = {
  { &str_type, 1},
};

base_obj *alloc_obj (type_obj *ob_type, size_t sz)
{
  base_obj *obj = (base_obj *)malloc (sz);
  if (!obj)
    return NULL;
  obj->ob_type = ob_type;
  obj->ob_refcnt = 1;
  return obj;
}

void unref (base_obj *obj)
{
  //__analyzer_dump();
  if (--obj->ob_refcnt == 0)
    free (obj);
}

void test_1 ()
{
  base_obj *obj = alloc_obj (&str_type, sizeof (string_obj));
  if (obj)
    {
      __analyzer_dump_num_heap_regions (); /* { dg-warning "num heap regions: '1'" } */
      unref (obj);
      __analyzer_dump_num_heap_regions (); /* { dg-warning "num heap regions: '0'" } */
    }
}
