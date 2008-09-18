// { dg-do assemble  }
// GROUPS passed conversions
class GttObject {};
class GctObject: virtual public GttObject {};
class NDAMObject: virtual public GttObject, virtual public GctObject {};
class GctHashObject: virtual public GctObject {};

class GctRef: virtual public GctHashObject
{ public: operator void*() const; };

class NDAMAssemblerObject: virtual public NDAMObject {};
class GctReferenceObject: virtual public GctHashObject {};
class NDAMValue: virtual public NDAMAssemblerObject, public GctReferenceObject {};

class nnyacc;
class NDAMValueRef : virtual public NDAMObject, public GctRef 
{
  NDAMValue *operator->() const;	 
  operator NDAMValue *() const;	 
friend class nnyacc;
};

typedef void* Pix;
class NDAMValueRefSLList
{
public:
  NDAMValueRefSLList();
  NDAMValueRefSLList(const NDAMValueRefSLList& a);
  ~NDAMValueRefSLList();
  NDAMValueRef& operator () (Pix p) const;
};

struct bar
{
  NDAMValueRefSLList *valueList;
};

class nnyacc
{
public:
      static void assign(void*& lval, void*& rval); // { dg-message "candidates" }
};

void
foo (bar yylval, bar *yyvsp)
{
  nnyacc::assign(yylval.valueList, yyvsp[0].valueList);// { dg-error "no matching" } 
}
