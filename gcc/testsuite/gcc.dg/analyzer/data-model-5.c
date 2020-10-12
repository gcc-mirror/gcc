/* A toy re-implementation of CPython's object model.  */

#include <stddef.h>
#include <string.h>
#include <stdlib.h>

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

typedef struct tuple_obj
{
  base_obj tup_base;
  int num_elements;
  base_obj elements[];
} tuple_obj;

typedef struct list_obj
{
  base_obj list_base;
  int num_elements;
  base_obj *elements;
} list_obj;

typedef struct string_obj
{
  base_obj str_base;
  size_t str_len;
  char str_buf[];
} string_obj;

extern void type_del (base_obj *);
extern void tuple_del (base_obj *);
extern void str_del (base_obj *);

type_obj type_type = {
  { &type_type, 1},
  type_del
};

type_obj tuple_type = {
  { &type_type, 1},
  tuple_del
};

type_obj str_type = {
  { &str_type, 1},
  str_del
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

base_obj *new_string_obj (const char *str)
{
  //__analyzer_dump ();
  size_t len = strlen (str);
#if 1
  string_obj *str_obj
    = (string_obj *)alloc_obj (&str_type, sizeof (string_obj) + len + 1);
#else
  string_obj *str_obj = (string_obj *)malloc (sizeof (string_obj) + len + 1);
  if (!str_obj)
    return NULL;
  str_obj->str_base.ob_type = &str_type;
  str_obj->str_base.ob_refcnt = 1;
#endif
  str_obj->str_len = len; /* { dg-warning "dereference of NULL 'str_obj'" } */
  memcpy (str_obj->str_buf, str, len);
  str_obj->str_buf[len] = '\0';
  return (base_obj *)str_obj;
}

void unref (base_obj *obj)
{
  if (--obj->ob_refcnt == 0) /* { dg-bogus "dereference of uninitialized pointer 'obj'" } */
    obj->ob_type->tp_dealloc (obj);
  /* { dg-warning "dereference of NULL 'obj'" "deref of NULL" { target *-*-* } .-2 } */
  /* FIXME: ideally we wouldn't issue this, as we've already issued a
     warning about str_obj which is now in the "stop" state; the cast
     confuses things.  */
}

void test_1 (const char *str)
{
  base_obj *obj = new_string_obj (str);
  unref (obj);
} /* { dg-bogus "leak" "" { xfail *-*-* } } */
/* XFAIL (false leak):
   Given that we only know "len" symbolically, this line:
     str_obj->str_buf[len] = '\0';
   is a symbolic write which could clobber the ob_type or ob_refcnt.
   It reports a leak when following the path where the refcount is clobbered
   to be a value that leads to the deallocator not being called.  */
