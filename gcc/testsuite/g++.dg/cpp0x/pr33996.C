// { dg-options "-std=c++0x" }

#define BUG
struct type
{
  type() { }
  type(const type&) { }

private:
  type(type&&);
};

template<typename _Tp>
  struct identity
  {
    typedef _Tp type;
  };

template<typename _Tp>
  inline _Tp&&
  forward(typename identity<_Tp>::type&& __t)
  { return __t; }

struct vec
{
  template<typename _Args>
    void
    bar(_Args&& __args)
#ifdef BUG
    ;
#else
    {
      type(forward<_Args>(__args));
    }
#endif
};

#ifdef BUG
template<typename _Args>
  void
  vec::bar(_Args&& __args)
  {
    type(forward<_Args>(__args));
  }
#endif

int main()
{
  vec v;
  type c;
  v.bar(c);
}
