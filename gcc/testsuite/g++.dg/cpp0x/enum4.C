// PR c++/40633
// { dg-do compile { target c++11 } }

template< typename T >
struct wrap {
   enum class E { val };
};

