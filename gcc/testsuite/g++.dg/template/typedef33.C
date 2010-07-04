// Origin PR c++/43800
// { dg-do compile }

template<class T, class U=T>
struct V
{
  typedef T t_type;
};

template<class T>
class J
{
  typedef typename V<T>::t_type t_type; 
  const t_type& f(); // #0:
private:
  t_type b;
};

template<class T>
const typename V<T>::t_type& J<T>::f() {return b;} // #1

