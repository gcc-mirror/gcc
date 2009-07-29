/* { dg-do run } */

extern "C" void abort (void);
class XalanDOMString
{
public:
  int y;
};

class XObject
{
public:
  const XalanDOMString& str() const { return x; }
  XalanDOMString x;
};

class XObjectPtr
{
public:
  XObjectPtr(const XObjectPtr& theSource)
    {
      m_xobjectPtr = theSource.m_xobjectPtr;
    }
  const XObject* operator->() const
    {
      return m_xobjectPtr;
    };
  XObjectPtr(XObject *p) { m_xobjectPtr = p; }
  XObject* m_xobjectPtr;
};

class FunctionSubstringBefore
{
public:
  int execute( const XObjectPtr arg1) const
    {
      const XalanDOMString& theFirstString = arg1->str();
      return theFirstString.y;
    }
};

int
main ()
{
  XObject x; 
  XObjectPtr y (&x);
  x.x.y = -1;
  FunctionSubstringBefore z;
  if (z.execute (y) != -1)
    abort ();
  return 0;
}
