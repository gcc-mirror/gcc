// PR tree-opt/18040
// { dg-do compile }
// { dg-options "-O3" }

int PyObject_IsTrue();
struct object_base
{
  void ptr() const;
  void ptr1() const;
};
struct object : public object_base
{
  typedef void (object::*bool_type)() const;
  inline operator bool_type() const
    { return PyObject_IsTrue()
       ? &object_base::ptr : &object::ptr1; }
};
void f();
void g (void)
{
    for (unsigned n = 0; n < 100; ++n)
    {
        object kv;
        if (kv)
          f();
    }
}

