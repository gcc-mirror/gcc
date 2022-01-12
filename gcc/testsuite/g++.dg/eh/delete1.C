// { dg-do run }
// { dg-xfail-run-if "AIX operator delete" { powerpc-ibm-aix* } }

// pr 55635, the delete operator must be called, regardless of whether
// the dtor throws

static int deleted;

void operator delete (void *) throw ()
{
  deleted = 1;
}

struct Foo {
  ~Foo()
#if __cplusplus <= 201402L
  throw(int)			// { dg-warning "deprecated" "" { target { c++11 && { ! c++17 } } } }
#else
  noexcept(false)
#endif
  {throw 1;}
};

struct Baz {
  void operator delete (void *) throw ()
  {
    deleted = 2;
  }
  virtual ~Baz()
#if __cplusplus <= 201402L
  throw(int)			// { dg-warning "deprecated" "" { target { c++11 && { ! c++17 } } } }
#else
  noexcept(false)
#endif
  {throw 1;}
};

int non_virt ()
{
  deleted = 0;
  
  Foo *p = new Foo;
  try { delete p; }
  catch (...) { return deleted != 1;}
  return 1;
}

int virt_glob ()
{
  deleted = 0;
  
  Baz *p = ::new Baz;
  try { ::delete p; }
  catch (...) { return deleted != 1;}
  return 1;
}

int virt_del ()
{
  deleted = 0;
  
  Baz *p = new Baz;
  try { delete p; }
  catch (...) { return deleted != 2;}
  return 1;
}

int ary ()
{
  deleted = 0;

  Baz *p = new Baz[1];
  try { delete[] p; }
  catch (...) { return deleted != 1;}
  return 1;
}

int main ()
{
  if (non_virt ())
    return 1;

  if (virt_glob ())
    return 2;

  if (virt_del ())
    return 3;

  if (ary ())
    return 4;
  
  return 0;
}
