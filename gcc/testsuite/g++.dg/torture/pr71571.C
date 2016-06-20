// { dg-do run }
// { dg-options "-fno-inline" { target { ! fpic } } }
// { dg-options "-fpic -fno-inline" { target fpic } }

class XBase
{
public:
 virtual void FuncA() = 0;
};

class Y
{
protected:
 virtual void FuncB() {}
};

class X1 : public Y, public XBase
{
public:
 void FuncA() {}
};

class X2 : public XBase
{
public:
 X2(XBase &xb) : m_xb(xb) { }
 void FuncA()
 {
  m_xb.FuncA();
 }

private:
 XBase &m_xb;
};


int main()
{
 X1 x1;
 X2 x2(x1);
 XBase *pxb = &x2;
 pxb->FuncA();
}
