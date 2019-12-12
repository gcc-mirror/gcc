// { dg-do compile }

class Signal
{
public:
    int  m_Mode;
};

class Ctx
{
public:
    bool  m_Invert;

    void DoSomething();
};

class Test
{
  void TestIce( Ctx& ctx, Signal* sig);
};

void Test::TestIce( Ctx& ctx, Signal* sig)
{
  int invert = false;

  if( ! ctx.m_Invert)
    invert = ! invert;

  switch( sig->m_Mode)
    {
    case 1:
    invert = ! invert;
    break;

    case 2:
    invert = true;
    break;
    }

  if( invert)
    ctx.DoSomething();
}
