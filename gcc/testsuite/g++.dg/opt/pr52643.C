// PR c++/52643
// { dg-options "-O" }

template<class T> class already_AddRefd {};

template<class T>
class ObjRef
{
public:
  ObjRef() {}

  ObjRef(const already_AddRefd<T> aar) {}

  ~ObjRef()
  {
    T* mPtr;
    mPtr->release_ref();
  }

  operator T* () const
  {
    return __null;
  }

  template<class U>
  void operator= (const already_AddRefd<U>& newAssign) {}
};

class MyRetClass {
public:
  void release_ref();
};

class MyClass
{
  void appendChild();
  void getTripleOutOfByPredicate();
  already_AddRefd<MyRetClass> getNextTriple();
};

void
MyClass::getTripleOutOfByPredicate()
{
  ObjRef<MyRetClass> t (getNextTriple());

  if (t == __null)
    throw MyRetClass();
}

void
MyClass::appendChild()
{
  while (1)
  {
    try
    {
      ObjRef<MyRetClass> t (getNextTriple());
      continue;
    }
    catch (MyRetClass)
    {
    }
  }
}
