// { dg-do compile }

class A
{
public:
    int m_fn1 ();
};
class B
{
  void m_fn2 (const int &p1);
  A mThebesLayerDataStack;
};
int b, c;
void B::m_fn2 (const int &p1)
{
  if (c && b)
    {
      int i;
      i = mThebesLayerDataStack.m_fn1 ();
      for (; i >= 0;)
	{
	  ++i;
	  break;
	}
      --i;
      for (; i >= 0; --i)
	mThebesLayerDataStack.m_fn1 ();
    }
}
