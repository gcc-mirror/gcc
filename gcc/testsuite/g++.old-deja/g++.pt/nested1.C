// Build don't link:

template <class T1,class T2>
struct X
{
  T1 a;

  struct Y
  {
    T2 x;
    Y (T2 _x) { x=_x; }
  };

};

template <class T1>
struct X<T1,int>
{
  T1 a;

  struct Y
  {
    int x;
    Y (int _x) { x=_x; }
  };

};

template <>
struct X<int,int>
{
  int a;

  struct Y
  {
    int x;
    Y (int _x) { x=_x; }
  };

};

void f ()
{
  X<char,char> t1;
  X<char,int> t2; 
  X<int,int> t3;  
}
