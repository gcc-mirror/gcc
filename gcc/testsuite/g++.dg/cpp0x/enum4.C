// PR c++/40633
// { dg-options "-std=c++11" }

template< typename T >
struct wrap {
   enum class E { val };
};

