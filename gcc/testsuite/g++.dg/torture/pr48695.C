// { dg-do run }

typedef __SIZE_TYPE__ size_t;

inline void *operator new (size_t, void *__p) throw() { return __p; }

struct _Vector_impl
{
  int *_M_start;
  int *_M_finish;
  _Vector_impl () :_M_start (0), _M_finish (0) {}
};

struct vector
{
  _Vector_impl _M_impl;
  int *_M_allocate (size_t __n)
  {
    return __n != 0 ? new int[__n] : 0;
  }
  void push_back ()
  {
    new (this->_M_impl._M_finish) int ();
    this->_M_impl._M_finish =
      this->_M_allocate (this->_M_impl._M_finish - this->_M_impl._M_start) + 1;
  }
};

int
main ()
{
  for (int i = 0; i <= 1; i++)
    for (int j = 0; j <= 1; j++)
      {
	vector a[2];
	a[i].push_back ();
      }
}
