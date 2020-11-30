/* Adapted from CPython 3.8's ceval.h.  */
typedef struct PyThreadState PyThreadState;
extern PyThreadState * PyEval_SaveThread(void);
extern void PyEval_RestoreThread(PyThreadState *);

#define Py_BEGIN_ALLOW_THREADS { \
                        PyThreadState *_save; \
                        _save = PyEval_SaveThread();
#define Py_BLOCK_THREADS        PyEval_RestoreThread(_save);
#define Py_UNBLOCK_THREADS      _save = PyEval_SaveThread();
#define Py_END_ALLOW_THREADS    PyEval_RestoreThread(_save); \
                 }

/* Adapted/hacked up from CPython 3.8's object.h.  */

typedef struct _object {
    int ob_refcnt;
} PyObject;

#define _PyObject_CAST(op) ((PyObject*)(op))

extern void _Py_Dealloc(PyObject *);

#define _Py_INCREF(OP) do { (OP)->ob_refcnt++; } while (0);
#define _Py_DECREF(OP) do {	  \
    if (--(OP)->ob_refcnt == 0) { \
      _Py_Dealloc(OP);		  \
    }				  \
  } while (0)

#define Py_INCREF(op) _Py_INCREF(_PyObject_CAST(op))
#define Py_DECREF(op) _Py_DECREF(_PyObject_CAST(op))
