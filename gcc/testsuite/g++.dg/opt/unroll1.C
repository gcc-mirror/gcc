// PR optimization/12340
// Origin: Richard Guenther <richard.guenther@uni-tuebingen.de>
// Testcase by Eric Botcazou <ebotcazou@libertysurf.fr>

// This used to segfault on x86 because the loop optimizer wrongly
// interpreted a double assignment to a biv as a double increment,
// which subsequently fooled the unroller.

// { dg-do run }
// { dg-options "-O2 -fno-exceptions -funroll-loops" }

typedef __SIZE_TYPE__ size_t;

inline void* operator new(size_t, void* __p) throw() { return __p; }
inline void operator delete (void*, void*) throw() { };

class Loc;
class Interval;

template<class DT>
class DomainBase
{
public:
  typedef typename DT::Domain_t Domain_t;
  typedef typename DT::Storage_t Storage_t;

  Domain_t &unwrap() { return *static_cast<Domain_t *>(this); }

  const Domain_t &unwrap() const {
    return *static_cast<Domain_t *>(const_cast<DomainBase<DT> *>(this));
  }

protected:
  Storage_t domain_m;
};

template<class DT>
class Domain : public DomainBase<DT>
{
  typedef DomainBase<DT> Base_t;

public:
  typedef typename DT::Size_t Size_t;
  typedef typename DT::Element_t Element_t;
  typedef typename Base_t::Domain_t Domain_t;
  typedef typename Base_t::Storage_t Storage_t;

  Domain_t &operator[](int) { return this->unwrap(); }

  const Domain_t &operator[](int) const { return this->unwrap(); }

  template<class T>
  void setDomain(const T &newdom) {
    DT::setDomain(this->domain_m, newdom);
  }

  Element_t first() const { return DT::first(this->domain_m); }

  Size_t length() const { return DT::length(this->domain_m); }

  Size_t size() const { return length(); }
};

template<class T>
struct DomainTraits;

template<>
struct DomainTraits<Interval>
{
  typedef int Size_t;
  typedef int Element_t;
  typedef Interval Domain_t;
  typedef Interval OneDomain_t;
  typedef Loc AskDomain_t;
  typedef int Storage_t[2];
  enum { dimensions = 1 };
  enum { wildcard = false };

  static int first(const Storage_t &d) { return d[0]; }

  static int length(const Storage_t &d) { return d[1]; }

  static OneDomain_t &getDomain(Domain_t &d, int) { return d; }

  static const OneDomain_t &getDomain(const Domain_t &d, int) { return d; }

  template<class T>
  static void setDomain(Storage_t &dom, const T &newdom) {
    dom[0] = newdom.first();  
    dom[1] = newdom.length();
  }

  template<class T1, class T2>
  static void setDomain(Storage_t &dom, const T1 &begval, const T2 &endval) {
    dom[0] = begval;
    dom[1] = (endval - begval + 1);
  }

};

class Interval : public Domain<DomainTraits<Interval> >
{
public:
  Interval(const Interval &a) : Domain<DomainTraits<Interval> >() {    
    for (int i=0; i < DomainTraits<Interval>::dimensions; ++i)
      DomainTraits<Interval>::getDomain(*this, i).setDomain(
                                DomainTraits<Interval>::getDomain(a, i));
  }

  Interval(int a) : Domain<DomainTraits<Interval> >()
  {
    DomainTraits<Interval>::setDomain(domain_m, 0, a - 1);
  }
};

template<>
struct DomainTraits<Loc>
{
  typedef int Size_t;
  typedef int Element_t;
  typedef Loc Domain_t;
  typedef Loc AskDomain_t;
  typedef Loc MultResult_t;
  typedef int Storage_t;

  static int first(int d) { return d; }

  template<class T>
  static void setDomain(int &dom, const T &newdom) {
    dom = DomainTraits<T>::getFirst(newdom);
  }
};

template<>
struct DomainTraits<int>
 {
  enum { dimensions = 1 };
  enum { wildcard = false };

  static int getPointDomain(int d, int) { return d; }

  static int getFirst(const int &d) { return d; }
};

class Loc : public Domain<DomainTraits<Loc> >
{
public:
  explicit Loc(const int &a) : Domain<DomainTraits<Loc> >() {
    for (int i=0; i < 1; ++i)
      (*this)[i].setDomain(DomainTraits<int>::getPointDomain(a, 0));
  }
};

struct ElementProperties
{
  enum { hasTrivialDefaultConstructor = false };
  enum { hasTrivialDestructor = false };

  static void construct(double* addr)
  {
    new (addr) double();
  }

  static void construct(double* addr, const double& model)
  {
    new (addr) double(model);
  }

  static void destruct(double *addr) {}
};

class RefCounted
{
public:
  RefCounted() : count_m(0) {}

  void addReference() { ++count_m; }
  bool removeRefAndCheckGarbage()
  {
    return (--count_m == 0);
  }

private:
  int count_m;
};

class RefBlockController : public RefCounted
{
public:
  explicit RefBlockController(unsigned int size)
    : pBegin_m(0), pEnd_m(0), pEndOfStorage_m(0), dealloc_m(false)
  {
    reallocateStorage(size, false);

    if (!ElementProperties::hasTrivialDefaultConstructor)
      {
        for (double * pt = begin(); pt != end(); ++pt)
          ElementProperties::construct(pt);
      }
  }
  
  ~RefBlockController()
  {
    deleteStorage();
  }

  double *begin() const
  {
    return pBegin_m;
  }

  double *end() const
  {
    return pEnd_m;
  }

  bool isMine() const
  {
    return dealloc_m;
  }

private:
  void deleteStorage()
  {
    if (isMine() && pBegin_m != 0)
      {
        if (!ElementProperties::hasTrivialDestructor)
          for (double *pt = begin(); pt != end(); ++pt)
            ElementProperties::destruct(pt);

        char *tmp = reinterpret_cast<char *>(pBegin_m);
        delete [] tmp;
      }
  }

  void reallocateStorage(unsigned int newsize, bool copyold = false)
  {
    double *pBeginNew = 0;
    double *pEndNew = 0;
    double *pEndOfStorageNew = 0;

    if (newsize > 0)
      {
        int nsize = newsize * sizeof(double);
        char *tmp = new char[nsize];
        pBeginNew = reinterpret_cast<double *>(tmp);
        pEndNew = pBeginNew + newsize;
        pEndOfStorageNew = pBeginNew + (nsize / sizeof(double));

        if (copyold)
          {
            double * pOld = begin();
            double * pNew = pBeginNew;
            while (pOld != end() && pNew != pEndNew)
              ElementProperties::construct(pNew++,*pOld++);
          }
      }

    deleteStorage();

    pBegin_m = pBeginNew;
    pEnd_m = pEndNew;
    pEndOfStorage_m = pEndOfStorageNew;
    dealloc_m = true;
  }

  double *pBegin_m;
  double *pEnd_m;
  double *pEndOfStorage_m;
  bool dealloc_m;
};

class DataBlockController : public RefBlockController
{
public:
  explicit
  DataBlockController(unsigned int size)
    : RefBlockController(size), dataObjectPtr_m(new char), owned_m(true) {}

  ~DataBlockController()
  {
    if (owned_m) delete dataObjectPtr_m;
  }

private:
  mutable char *dataObjectPtr_m;
  bool owned_m;
};

class RefCountedPtr
{
public:
  RefCountedPtr(DataBlockController * const pT) : ptr_m(pT)
    { if (isValid()) ptr_m->addReference(); }

  ~RefCountedPtr() { invalidate(); }

  DataBlockController* operator->() const { return ptr_m; }
  void invalidate();
  bool isValid() const { return ptr_m != 0; }

private:
  friend class RefCountedBlockPtr;
  DataBlockController * ptr_m;
};

inline void RefCountedPtr::invalidate()
{
  if ( isValid() && ptr_m->removeRefAndCheckGarbage() )
    delete ptr_m;
  ptr_m = 0;
}

class RefCountedBlockPtr
{
public:
  explicit RefCountedBlockPtr(unsigned int size)
    : offset_m(0),
      blockControllerPtr_m(new DataBlockController(size)) {}

  int offset() const
  {
    return offset_m;
  }

  double *beginPointer() const
  {
    return blockControllerPtr_m->begin();
  }

  double *currentPointer() const
  {
    return beginPointer() + offset();
  }

protected:
  int offset_m;
  RefCountedPtr blockControllerPtr_m;
};

class DataBlockPtr : public RefCountedBlockPtr
{
public:
  explicit DataBlockPtr(unsigned int size) : RefCountedBlockPtr(size) {}
};

class Node
{
public:
  Node(const Interval &owned, const Interval &allocated)
    : domain_m(owned), allocated_m(allocated) {}

  const Interval &allocated() const { return allocated_m; }

private:
  Interval domain_m;
  Interval allocated_m;
};

class DomainLayout
{
public:
  explicit DomainLayout(const Interval &dom) : node_m(0, dom) {}

  const Interval &domain() const
  {
    return node_m.allocated();
  }

private:
  Node node_m;
};

class BrickBase
{
public:
  explicit BrickBase(const Interval &domain);

  int offset(const Loc &dom) const { return off_m + dom[0].first(); }

protected:
  DomainLayout layout_m;
  int firsts_m;
  int off_m;
};

BrickBase::BrickBase(const Interval &dom)
  : layout_m(dom)
{
  firsts_m = layout_m.domain()[0].first();
  off_m = -firsts_m;
}

class Engine : public BrickBase
{
public:
  explicit Engine(const Interval &dom)
  : BrickBase(dom), dataBlock_m(dom.size()), data_m(dataBlock_m.currentPointer()) {}

  double& operator()(const Loc &loc) const
  {
    return data_m[this->offset(loc)];
  }

private:
  DataBlockPtr dataBlock_m;
  double *data_m;
};


int main()
{
  Interval I(10);
  Engine A(I);

  for (int i = 0; i < 10; i++)
    A(Loc(i)) = 2.0 + i - i*i;

  return 0;
}
