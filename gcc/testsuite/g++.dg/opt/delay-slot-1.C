/* PR rtl-optimization/23585 */
/* Testcase by Matti Rintala <matti.rintala@iki.fi> */

/* { dg-do run } */
/* { dg-options "-O2" } */

template <class _Ret, class _Tp>
class const_mem_fun_t
{
public:
  explicit
  const_mem_fun_t(_Ret (_Tp::*__pf)() const)
    : _M_f(__pf) {}
  
  _Ret
  operator()(const _Tp* __p) const
  { return (__p->*_M_f)(); }
private:
  _Ret (_Tp::*_M_f)() const;
};

template <class _Ret, class _Tp>
class const_mem_fun_ref_t
{
public:
  explicit
  const_mem_fun_ref_t(_Ret (_Tp::*__pf)() const)
    : _M_f(__pf) {}
  
  _Ret
  operator()(const _Tp& __r) const
  { return (__r.*_M_f)(); }
private:
  _Ret (_Tp::*_M_f)() const;
};

template <class _Ret, class _Tp, class _Arg>
class const_mem_fun1_t
{
public:
  explicit
  const_mem_fun1_t(_Ret (_Tp::*__pf)(_Arg) const)
    : _M_f(__pf) {}
  
  _Ret
  operator()(const _Tp* __p, _Arg __x) const
  { return (__p->*_M_f)(__x); }
private:
  _Ret (_Tp::*_M_f)(_Arg) const;
};


template <class _Ret, class _Tp, class _Arg>
class const_mem_fun1_ref_t
{
public:
  explicit
  const_mem_fun1_ref_t(_Ret (_Tp::*__pf)(_Arg) const)
    : _M_f(__pf) {}
  
  _Ret
  operator()(const _Tp& __r, _Arg __x) const
  { return (__r.*_M_f)(__x); }
private:
  _Ret (_Tp::*_M_f)(_Arg) const;
};

template <class _Ret, class _Tp>
inline const_mem_fun_t<_Ret, _Tp>
mem_fun(_Ret (_Tp::*__f)() const)
{ return const_mem_fun_t<_Ret, _Tp>(__f); }

template <class _Ret, class _Tp>
inline const_mem_fun_ref_t<_Ret, _Tp>
mem_fun_ref(_Ret (_Tp::*__f)() const)
{ return const_mem_fun_ref_t<_Ret, _Tp>(__f); }

template <class _Ret, class _Tp, class _Arg>
inline const_mem_fun1_t<_Ret, _Tp, _Arg>
mem_fun(_Ret (_Tp::*__f)(_Arg) const)
{ return const_mem_fun1_t<_Ret, _Tp, _Arg>(__f); }

template <class _Ret, class _Tp, class _Arg>
inline const_mem_fun1_ref_t<_Ret, _Tp, _Arg>
mem_fun_ref(_Ret (_Tp::*__f)(_Arg) const)
{ return const_mem_fun1_ref_t<_Ret, _Tp, _Arg>(__f); }

class Class {
public:
  void vf0c() const;
  void vf1c(const int&) const;
};

int main()
{
  Class obj;
  const Class& objc = obj;

  mem_fun(&Class::vf0c)(&objc);
  mem_fun(&Class::vf1c)(&objc, 1);

  mem_fun_ref(&Class::vf0c)(objc);
  mem_fun_ref(&Class::vf1c)(objc, 1);
  return 0;
}

void Class::vf0c() const
{}

void Class::vf1c(const int&) const
{}
