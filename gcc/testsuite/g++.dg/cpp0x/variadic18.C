// { dg-do compile { target c++11 } }
template<typename...> class tuple { };

template<typename T, template<typename U> class... Metafunctions>
struct apply_all
{
  typedef tuple<typename Metafunctions<T>::type...> type;
};

template<typename T, typename U>
struct is_same {
  static const bool value = false;
};

template<typename T>
struct is_same<T, T> {
  static const bool value = true;
};

template<typename T>
struct add_pointer {
  typedef T* type;
};

template<typename T>
struct add_pointer<T&>
{
  typedef T& type;
};

template<typename T>
struct add_reference {
  typedef T& type;
};

template<typename T>
struct add_reference<T&>
{
  typedef T& type;
};

int a0[is_same<apply_all<int>::type,tuple<> >::value? 1 : -1];
int a1[is_same<apply_all<int, add_pointer>::type,tuple<int*> >::value? 1 : -1];
int a2[is_same<apply_all<int, add_pointer, add_reference>::type,tuple<int*, int&> >::value? 1 : -1];
int a3[is_same<apply_all<int&, add_pointer, add_reference>::type,tuple<int&, int&> >::value? 1 : -1];


