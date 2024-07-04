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

/* { dg-begin-multiline-output "" }
[{"kind": "error",
  "message": "passing NULL as argument 1 to 'PyList_Append' which requires a non-NULL parameter",
  "children": [],
  "column-origin": 1,
  "locations": [{"caret": {"file": "
                           "line": 29,
                           "display-column": 5,
                           "byte-column": 5,
                           "column": 5},
                 "finish": {"file": "
                            "line": 29,
                            "display-column": 29,
                            "byte-column": 29,
                            "column": 29}}],
  "path": [{"location": {"file": "
                         "line": 25,
                         "display-column": 10,
                         "byte-column": 10,
                         "column": 10},
            "description": "when 'PyList_New' fails, returning NULL",
            "function": "make_a_list_of_random_ints_badly",
            "depth": 0},
           {"location": {"file": "
                         "line": 27,
                         "display-column": 17,
                         "byte-column": 17,
                         "column": 17},
            "description": "when 'i < count'",
            "function": "make_a_list_of_random_ints_badly",
            "depth": 0},
           {"location": {"file": "
                         "line": 29,
                         "display-column": 5,
                         "byte-column": 5,
                         "column": 5},
            "description": "when calling 'PyList_Append', passing NULL from (1) as argument 1",
            "function": "make_a_list_of_random_ints_badly",
            "depth": 0}],
  "escape-source": false}]
{ dg-end-multiline-output "" } */
