// { dg-options "-std=c++0x" }
template<int I>
struct accept_evens {
  static_assert( I % 2 == 0, "I must be an even number"); // { dg-error "even number" }
};

template<int I>
struct accept_evens_ok {
  static_assert( I % 2 == 0, "I must be an even number");
};

template<int I>
void accept_odds() {
  static_assert( I % 2 == 1, "I must be an odd number"); // { dg-error "odd number" }
}

template<int I>
void accept_odds_ok() {
  static_assert( I % 2 == 1, "I must be an odd number");
}

void f()
{
  accept_odds<1>();
  accept_odds<2>(); 
  accept_odds<3>();
  accept_odds_ok<5>();
  accept_odds_ok<7>();
}

accept_evens<0> ok0;
accept_evens<1> error1; 
accept_evens<2> ok2;
accept_evens_ok<4> ok4;
accept_evens_ok<6> ok6;
