// PR middle-end/71308
// { dg-do compile }

class S
{
  void foo ();
  virtual void bar () = 0;
  virtual ~S ();
};
inline void
S::foo ()
{
  bar ();
};
S::~S ()
{
  foo ();
}
