/* PR ipa/63470.C */
/* { dg-do compile } */
/* { dg-options "-O2 -finline-functions" } */

class A
{
public:
  virtual bool m_fn1 ();
  virtual const char **m_fn2 (int);
  virtual int m_fn3 ();
};
class FTjackSupport : A
{
  ~FTjackSupport ();
  bool m_fn1 ();
  bool m_fn4 ();
  const char **
  m_fn2 (int)
  {
  }
  int _inited;
  int *_jackClient;
  int _activePathCount;
}

* a;
void fn1 (...);
void fn2 (void *);
int fn3 (int *);
FTjackSupport::~FTjackSupport () { m_fn4 (); }

bool
FTjackSupport::m_fn1 ()
{
  if (!_jackClient)
    return 0;
  for (int i=0; _activePathCount; ++i)
    if (m_fn2 (i))
      fn2 (a);
  if (m_fn3 ())
    fn2 (a);
  if (fn3 (_jackClient))
    fn1 (0);
}

bool
FTjackSupport::m_fn4 ()
{
  if (_inited && _jackClient)
    {
      m_fn1 ();
      return 0;
    }
}
