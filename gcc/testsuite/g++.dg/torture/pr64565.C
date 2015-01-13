/* { dg-do compile } */
typedef enum
{
  NS_OK
} nsresult;
struct A
{
  static int kIID;
};
class B
{
};
class C
{
public:
  C (B p1) { m_fn1 (p1, A::kIID); }
  void m_fn1 (B, int);
};
class D;
class F
{
public:
  F (int);
};
class G
{
public:
  D *operator[](int);
};
class H
{
  virtual nsresult m_fn2 ();

public:
  void m_fn3 ();
};
class J : H
{
  G mQueries;
  int mLiveUpdate;
  nsresult m_fn2 ();
};
class D
{
public:
  nsresult m_fn4 (int);
  void m_fn5 (int);
};
class I
{
public:
  static I *
  m_fn6 ()
  {
    B __trans_tmp_3;
    if (!gHistoryService)
      C serv = __trans_tmp_3;
  }
  void m_fn7 ();
  static I *gHistoryService;
};
D *Refresh___trans_tmp_2;
D Refresh___trans_tmp_6, Refresh___trans_tmp_5;
int Refresh_hasDomain;
nsresult
J::m_fn2 ()
{
  m_fn3 ();
  I history = *I::m_fn6 ();
  switch (mLiveUpdate)
    {
    case 1:
      {
        mQueries[0];
        F query = 0;
        if (Refresh_hasDomain)
          return NS_OK;
      }
    case 0:
      {
        Refresh___trans_tmp_2 = mQueries[0];
        F query = Refresh___trans_tmp_5.m_fn4 (0);
        history.m_fn7 ();
        Refresh___trans_tmp_6.m_fn5 (0);
      }
    case 3:
      m_fn2 ();
    }
}
