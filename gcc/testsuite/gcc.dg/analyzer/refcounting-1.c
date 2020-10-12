#include "analyzer-decls.h"

typedef struct obj {
  int ob_refcnt;
} PyObject;

extern void Py_Dealloc (PyObject *op);

#define Py_INCREF(op)			\
  do {					\
    ((PyObject*)(op))->ob_refcnt++;	\
  } while (0)

#define Py_DECREF(op)                                   \
    do {                                                \
      if (--((PyObject*)(op))->ob_refcnt == 0)		\
	{						\
	  /*Py_Dealloc((PyObject *)(op));*/		\
	}						\
    } while (0)

void test_1 (PyObject *obj)
{
  int orig_refcnt = obj->ob_refcnt;
  Py_INCREF (obj);
  Py_INCREF (obj);
  Py_DECREF (obj);
  Py_INCREF (obj);
  __analyzer_eval (obj->ob_refcnt == orig_refcnt + 2); /* { dg-warning "TRUE" } */
}
/* TODO: uncomment the Py_Dealloc, which leads to two paths.  */
