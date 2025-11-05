// PR c++/122421
// { dg-additional-options "-fmodules" }
// { dg-module-cmi M }

export module M;

export template <typename T> struct Type {
  static const int arr[3];
};

extern template const int Type<double>::arr[3];
template <typename T> const int Type<T>::arr[] = { 42, 43, 44 };

export Type<int> ti;
