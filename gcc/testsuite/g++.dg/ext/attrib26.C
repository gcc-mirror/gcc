// PR c++/28659
// The attribute was causing us to get confused in merge_types when
// combining the template type with an uninstantiated version.

template<class T>
struct __attribute__((aligned(1))) A
{
  A& operator=(const A &t);
};

template<class T>
A<T>& A<T>::operator=(const A<T> &t)
{
}
