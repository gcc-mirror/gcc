// Build don't link:
// prms-id: 3538

// This tests for an ambiguous conversion of the this pointer (going
// down to DECL_CONTEXT of a FUNCTION_DECL.

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
{ return p->repInvariant(1); }

class ccHandleBase : public ccObjectInfo
{};

class cc_CircleHdl : public virtual ccHandleBase, public ccObjectInfo
{
public:
  virtual const ccObjectInfo& repInvariant (int);
};				// WARNING - 

class ccCircleHdl : public ccHandle <cc_CircleHdl> {};
