/* { dg-do compile } */
/* { dg-require-effective-target analyzer } */
/* { dg-options "-fanalyzer" } */
/* { dg-require-python-h "" } */


#define PY_SSIZE_T_CLEAN
#include <Python.h>
#include "../analyzer/analyzer-decls.h"

PyObject *
test_PyListAppend (long n)
{
  PyObject *item = PyLong_FromLong (n);
  PyObject *list = PyList_New (0);
  PyList_Append(list, item);
  return list; /* { dg-warning "leak of 'item'" } */
  /* { dg-warning "expected 'item' to have reference count" "" { target *-*-* } .-1 } */
}

PyObject *
test_PyListAppend_2 (long n)
{
  PyObject *item = PyLong_FromLong (n);
  if (!item)
	return NULL;

  __analyzer_eval (item->ob_refcnt == 1); /* { dg-warning "TRUE" } */
  PyObject *list = PyList_New (n);
  if (!list)
  {
	Py_DECREF(item);
	return NULL;
  }

  __analyzer_eval (list->ob_refcnt == 1); /* { dg-warning "TRUE" } */

  if (PyList_Append (list, item) < 0)
    __analyzer_eval (item->ob_refcnt == 1); /* { dg-warning "TRUE" } */
  else
    __analyzer_eval (item->ob_refcnt == 2); /* { dg-warning "TRUE" } */
  return list; /* { dg-warning "leak of 'item'" } */
  /* { dg-warning "expected 'item' to have reference count" "" { target *-*-* } .-1 } */
}


PyObject *
test_PyListAppend_3 (PyObject *item, PyObject *list)
{
  PyList_Append (list, item);
  return list;
}

PyObject *
test_PyListAppend_4 (long n)
{
  PyObject *item = PyLong_FromLong (n);
  PyObject *list = NULL;
  PyList_Append(list, item);
  return list;
}

PyObject *
test_PyListAppend_5 ()
{
  PyObject *list = PyList_New (0);
  PyList_Append(list, NULL);
  return list;
}

PyObject *
test_PyListAppend_6 ()
{
  PyObject *item = NULL;
  PyObject *list = NULL;
  PyList_Append(list, item);
  return list;
}