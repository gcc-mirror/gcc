// { dg-options "-std=c++1z -fconcepts" }

template<typename T, typename U>
struct is_same
{
  static constexpr bool value = false;
};

template<typename T>
struct is_same<T, T>
{
  static constexpr bool value = true;
};

template<class T, class U>
concept bool Same = is_same<T, U>::value;

template<typename T>
concept bool C1 = true;

template<typename T, typename U>
concept bool C2 = true;

template<typename T>
concept bool C3() { return true; }

template<typename T, typename U>
concept bool C4() { return true; }

C1      c1 = 0;
C2<int> c2 = 0;
C3      c3 = 0;
C4<int> c4 = 0;
Same<int> s1 = 'a'; // { dg-error "does not satisfy|is_same" }
