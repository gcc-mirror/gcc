// PR c++/40633
// { dg-options "-std=c++0x" }

template< typename T >
struct wrap {
   enum class E { val };
};

