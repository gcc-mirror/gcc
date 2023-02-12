// PR c++/108090
// { dg-do compile { target c++17 } }

template<typename T> struct As { operator T(); };
template<typename ...T> struct AsAll : As<T>... {
  using As<T>::operator T...;
};
AsAll<int, float, char> x;
