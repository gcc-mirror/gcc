// Build don't link: 
// Origin: Mark Mitchell <mark@codesourcery.com>
// Special g++ Options: -O3

class ostream;

struct S
{
  virtual void print(ostream&) const = 0;
};

template <class _Tp>
class vector
{
public:
  _Tp& operator[](unsigned __n) const { return *(_M_start + __n); }
  _Tp* _M_start;
};

class T
{
public:

  void print(ostream&) const;

  vector<S*> bcList_m;
};

void T::print(ostream& o) const
{
  int n = 3;

  for (int i = 0; i < n; ++i)
    bcList_m[i]->print(o);
  return;
}

ostream&
operator<<(ostream& o, const T& bcList)
{
  bcList.print(o);
  return o;
}


 
 
 
 
 
 
