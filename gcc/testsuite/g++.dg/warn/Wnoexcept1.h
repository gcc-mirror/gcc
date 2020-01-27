// -*- C++ -*-
#pragma GCC system_header

using size_t = decltype(sizeof(42));
inline void * operator new (size_t, void *p) noexcept { return p; }

template<typename _Up, typename... _Args>
void
construct1(_Up* __p, _Args... __args)
  noexcept(noexcept(::new((void *)__p) _Up(__args...)))
{ ::new((void *)__p) _Up(__args...); }

template<typename _Up, typename... _Args>
void
construct2(_Up* __p, _Args... __args)
  noexcept(noexcept(::new((void *)__p) _Up(__args...)))
{ ::new((void *)__p) _Up(__args...); }

class Automatic1 {
public:
  Automatic1(size_t bla) : Bla(bla) {};

private:
  size_t Bla;
};
