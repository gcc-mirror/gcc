typedef int PyObject;
typedef PyObject *(*PyCFunction)(PyObject *, PyObject *);
template<class T> int _clear(PyObject* self);

void _typeInfo() 
{
  reinterpret_cast<PyCFunction>(_clear); // { dg-error "overloaded function" }
}
