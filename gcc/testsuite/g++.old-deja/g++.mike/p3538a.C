// Build don't link:
// This tests for the compiler_error in binfo_value.
// prms-id: 3538

class ccObjectInfo
{
public:
    virtual const ccObjectInfo& repInvariant (int);
};

template<class T>
class ccHandle : public  ccObjectInfo
{
protected:
  T* p;
public:
  virtual const ccObjectInfo& repInvariant (int);
};

template <class T>
const ccObjectInfo& ccHandle<T>::repInvariant (int)
{ return p->ri(1); }

class ccHandleBase : public ccObjectInfo
{};

class cc_CircleHdl : public virtual ccHandleBase, public ccObjectInfo
{
public:
  virtual const ccObjectInfo& ri (int);
};				// WARNING - 

class ccCircleHdl : public ccHandle <cc_CircleHdl> {};
