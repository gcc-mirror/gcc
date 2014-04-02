// PR c++/40639
// { dg-do compile { target c++11 } }

template< typename T >
struct wrap {
   enum E : T { val };
};

template< typename T >
struct dependant {
   enum E : typename T::type { val };
};

template<typename T>
struct identity {
   typedef T type;
};

wrap<int> x;
dependant<identity<int>> y;
