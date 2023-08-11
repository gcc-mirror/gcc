/* { dg-do compile } */
/* { dg-require-effective-target analyzer } */
/* { dg-options "-fanalyzer" } */
/* { dg-require-python-h "" } */


#define PY_SSIZE_T_CLEAN
#include <Python.h>
#include "../analyzer/analyzer-decls.h"

PyObject *
test_PyList_New (Py_ssize_t len)
{
  PyObject *obj = PyList_New (len);
  if (obj)
    {
     __analyzer_eval (obj->ob_refcnt == 1); /* { dg-warning "TRUE" } */
     __analyzer_eval (PyList_CheckExact (obj)); /* { dg-warning "TRUE" } */
    }
  else
    __analyzer_dump_path (); /* { dg-message "path" } */
  return obj;
}

PyObject *
test_PyLong_New (long n)
{
  PyObject *obj = PyLong_FromLong (n);
  if (obj)
    {
     __analyzer_eval (obj->ob_refcnt == 1); /* { dg-warning "TRUE" } */
     __analyzer_eval (PyLong_CheckExact (obj)); /* { dg-warning "TRUE" } */
    }
  else
    __analyzer_dump_path (); /* { dg-message "path" } */
  return obj;
}

PyObject *
test_PyListAppend (long n)
{
  PyObject *item = PyLong_FromLong (n);
  PyObject *list = PyList_New (0);
  PyList_Append(list, item);
  return list; /* { dg-warning "leak of 'item'" } */
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
}


PyObject *
test_PyListAppend_3 (PyObject *item, PyObject *list)
{
  PyList_Append (list, item);
  return list;
}