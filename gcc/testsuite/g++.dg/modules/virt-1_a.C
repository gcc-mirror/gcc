// { dg-module-do run }
// { dg-additional-options -fmodules-ts }
export module foo;
// { dg-module-cmi foo }

export struct Visitor
{
  virtual int Visit ()
  {
    return 0;
  }
};

export int Visit (Visitor *v)
{
  return v->Visit ();
}

