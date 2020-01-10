/* { dg-do compile } */
/* { dg-options "-fdiagnostics-path-format=separate-events" } */

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

  list = PyList_New(0); /* { dg-line PyList_New } */
	
  for (i = 0; i < count; i++) { /* { dg-line for } */
    item = PyLong_FromLong(random());
    PyList_Append(list, item); /* { dg-line PyList_Append } */
  }
  
  return list;

  /* { dg-error "passing NULL as argument 1 to 'PyList_Append' which requires a non-NULL parameter" "" { target *-*-* } PyList_Append } */
  /* { dg-message "\\(1\\) when 'PyList_New' fails, returning NULL" "" { target *-*-* } PyList_New } */
  /* { dg-message "\\(2\\) when 'i < count'" "" { target *-*-* } for } */
  /* { dg-message "\\(3\\) when calling 'PyList_Append', passing NULL from \\(1\\) as argument 1" "" { target *-*-* } PyList_Append } */
}
