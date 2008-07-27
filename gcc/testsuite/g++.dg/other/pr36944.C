/* { dg-do compile } */

class XObject
{
public:
  int foo;
};

class XObjectPtr
{
public:
 explicit
 XObjectPtr(XObject* theXObject = 0) : m_xobjectPtr(theXObject)
 {
 }

private:
 XObject * m_xobjectPtr;
};

class SelectionEvent
{
public:
 SelectionEvent(bool selection) : m_selection() {}
 const XObjectPtr m_selection;
};
