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

void
test_PyList_New_2 ()
{
  PyObject *obj = PyList_New (0);
} /* { dg-warning "leak of 'obj'" } */

PyObject *test_stray_incref_PyList ()
{
  PyObject *p = PyList_New (2);
  if (p)
    Py_INCREF (p);
  return p;
  /* { dg-warning "expected 'p' to have reference count" "" { target *-*-* } .-1 } */
}