#include <stdlib.h>

/* Ensure that we don't get an exponential growth in paths due to
   repeated diamonds in the CFG.  */

typedef struct obj {
  int ob_refcnt;
} PyObject;

extern void Py_Dealloc (PyObject *op);

#define Py_DECREF(op)                                   \
    do {                                                \
      if (--((PyObject*)(op))->ob_refcnt == 0)		\
	Py_Dealloc((PyObject *)(op));			\
    } while (0)

int test (PyObject *obj_01, PyObject *obj_02, PyObject *obj_03,
	  PyObject *obj_04, PyObject *obj_05, PyObject *obj_06,
	  PyObject *obj_07, PyObject *obj_08, PyObject *obj_09,
	  PyObject *obj_10, PyObject *obj_11, PyObject *obj_12,
	  PyObject *obj_13, PyObject *obj_14, PyObject *obj_15
)
{
  Py_DECREF (obj_01); Py_DECREF (obj_02); Py_DECREF (obj_03);
  Py_DECREF (obj_04); Py_DECREF (obj_05); Py_DECREF (obj_06);
  Py_DECREF (obj_07); Py_DECREF (obj_08); Py_DECREF (obj_09);
  Py_DECREF (obj_10); Py_DECREF (obj_11); Py_DECREF (obj_12);
  Py_DECREF (obj_13); Py_DECREF (obj_14); Py_DECREF (obj_15);
}
