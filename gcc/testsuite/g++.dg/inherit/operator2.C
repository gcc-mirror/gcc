typedef int INT_TYPEDEF;

template<class T>
class TypedIfc
{
public:
  virtual ~TypedIfc() { }
  virtual operator const T&() const = 0;
  virtual const T& operator= (const T& t) = 0;
};

template<class Tnative>
class NullIfc : public TypedIfc<Tnative>
{
public:
  const Tnative& operator= (const Tnative& t) { return t; }
  operator const Tnative&() const { return *(Tnative *)0; }
};

typedef TypedIfc<INT_TYPEDEF> INT_TYPEDEFIfc;

NullIfc<int> i32;
