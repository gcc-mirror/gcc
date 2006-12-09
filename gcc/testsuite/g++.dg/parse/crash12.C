// { dg-do compile }

// Origin: Martin von Loewis <martin@v.loewis.de>

// PR c++/157: Incorrect type/template decision in function parameter.

template <class _Tp> class auto_ptr {};
template <class _Tp>
class counted_ptr
{
public:
  counted_ptr(::auto_ptr<_Tp>& __a);		// { dg-error "candidate" }
  ::auto_ptr<_Tp> auto_ptr();
};

template <class _Tp>
inline counted_ptr<_Tp>::counted_ptr(class auto_ptr& __a) // { dg-error "required|not match|template" }
{
}

template <class _Tp>
inline class auto_ptr<_Tp> counted_ptr<_Tp>::auto_ptr() 
{
}
