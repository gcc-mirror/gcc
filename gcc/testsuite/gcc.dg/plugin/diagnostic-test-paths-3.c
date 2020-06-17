/* { dg-do compile } */
/* { dg-options "-fdiagnostics-format=json" } */

#include <stddef.h>
#include <stdlib.h>

/* Minimal reimplementation of cpython API.  */
typedef struct PyObject {} PyObject;
extern int PyArg_ParseTuple (PyObject *args, const char *fmt, ...);
extern PyObject *PyList_New (int);
extern PyObject *PyLong_FromLong(long);
extern void PyList_Append(PyObject *list, PyObject *item);

PyObject *
make_a_list_of_random_ints_badly(PyObject *self,
				 PyObject *args)
{
  PyObject *list, *item;
  long count, i;

  if (!PyArg_ParseTuple(args, "i", &count)) {
    return NULL;
  }

  list = PyList_New(0);
	
  for (i = 0; i < count; i++) {
    item = PyLong_FromLong(random());
    PyList_Append(list, item);
  }
  
  return list;
}

/* FIXME: test the events within a path.  */
/* { dg-regexp "\"kind\": \"error\"" } */
/* { dg-regexp "\"path\": " } */
/* { dg-regexp ".*" } */
