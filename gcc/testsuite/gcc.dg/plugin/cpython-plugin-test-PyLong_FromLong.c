/* { dg-do compile } */
/* { dg-require-effective-target analyzer } */
/* { dg-options "-fanalyzer" } */
/* { dg-require-python-h "" } */


#define PY_SSIZE_T_CLEAN
#include <Python.h>
#include "../analyzer/analyzer-decls.h"

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

void
test_PyLong_New_2 (long n)
{
  PyObject *obj = PyLong_FromLong (n);
} /* { dg-warning "leak of 'obj'" } */

PyObject *test_stray_incref_PyLong (long val)
{
  PyObject *p = PyLong_FromLong (val);
  if (p)
    Py_INCREF (p);
  return p;
  /* { dg-warning "expected 'p' to have reference count" "" { target *-*-* } .-1 } */
}