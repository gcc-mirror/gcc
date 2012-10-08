// PR c++/28659
// The attribute was causing us to get confused in merge_types when
// combining the template type with an uninstantiated version.
// { dg-do compile { target c++11 } }

template<class T>
struct [[gnu::aligned(1)]] A
{
  A& operator=(const A &t);
};

template<class T>
A<T>& A<T>::operator=(const A<T> &t)
{
}
