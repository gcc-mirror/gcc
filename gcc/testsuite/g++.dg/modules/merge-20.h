// PR c++/122422
template <typename> struct Type {
  constexpr static int arr[] = { 42, 43, 44 };
};
inline Type<int> tt;
