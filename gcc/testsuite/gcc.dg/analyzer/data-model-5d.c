/* A toy re-implementation of CPython's object model.  */

#include <stddef.h>
#include <string.h>
#include <stdlib.h>
#include "analyzer-decls.h"

typedef struct base_obj
{
  struct type_obj *ob_type;
  int ob_refcnt;
} base_obj;

typedef struct type_obj
{
  base_obj tp_base;
  void (*tp_dealloc) (base_obj *);
} type_obj;

typedef struct boxed_int_obj
{
  base_obj int_base;
  int int_val;
} boxed_int_obj;

extern void int_del (base_obj *);

type_obj type_type = {
  { &type_type, 1}
};

type_obj boxed_int_type = {
  { &type_type, 1},
  int_del
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

base_obj *new_int_obj (int val)
{
  boxed_int_obj *int_obj
    = (boxed_int_obj *)alloc_obj (&boxed_int_type, sizeof (boxed_int_obj));
  if (!int_obj)
    return NULL;
  int_obj->int_val = val;
  return (base_obj *)int_obj;
}

void unref (base_obj *obj)
{
  if (--obj->ob_refcnt == 0)
    obj->ob_type->tp_dealloc (obj);
}

void test_1 (const char *str)
{
  base_obj *obj = new_int_obj (42);
  if (!obj)
    return;
  __analyzer_eval (((boxed_int_obj *)obj)->int_val == 42); /* { dg-warning "TRUE" } */
  __analyzer_eval (obj->ob_refcnt == 1); /* { dg-warning "TRUE" } */
  unref (obj);
} /* { dg-bogus "leak" "" } */
