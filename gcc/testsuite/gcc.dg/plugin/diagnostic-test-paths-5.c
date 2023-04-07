/* { dg-do compile } */
/* { dg-options "-fdiagnostics-format=sarif-file" } */
/* { dg-excess-errors "The error is sent to the SARIF file, rather than stderr" } */

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

/* 
   { dg-final { verify-sarif-file } }

   { dg-final { scan-sarif-file "\"tool\": " } }

     We expect info about the plugin:
     { dg-final { scan-sarif-file "\"extensions\": \\\[" } }
       { dg-final { scan-sarif-file "\"name\": \"diagnostic_plugin_test_paths\"" } }
       { dg-final { scan-sarif-file "\"fullName\": \"" } }

     { dg-final { scan-sarif-file "\"results\": \\\[" } }
       { dg-final { scan-sarif-file "\"level\": \"error\"" } }
         { dg-final { scan-sarif-file "\"text\": \"passing NULL as argument 1 to 'PyList_Append' which requires a non-NULL parameter\"" } }

       We expect a path for the diagnostic:
       { dg-final { scan-sarif-file "\"codeFlows\": \\\[" } }
         { dg-final { scan-sarif-file "\"threadFlows\": \\\[" } }
           { dg-final { scan-sarif-file "\"locations\": \\\[" } }
             { dg-final { scan-sarif-file "\"text\": \"when 'PyList_New' fails, returning NULL\"" } }
             { dg-final { scan-sarif-file "\"text\": \"when 'i < count'\"" } }
             { dg-final { scan-sarif-file "\"text\": \"when calling 'PyList_Append', passing NULL from \\(1\\) as argument 1\"" } }

*/
