// { dg-do assemble  }

template <class T>
struct U
{
  T mT;
};
 
template <class H>
struct M
{
  enum FLAG {On, Off};
  U<FLAG> mUF;
};

M<char> gm;
