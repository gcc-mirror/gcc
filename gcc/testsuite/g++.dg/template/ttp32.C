// PR c++/83916
// { dg-do compile { target c++17 } }

template<class TA,
	 template<auto...> class TTA, TA... VA>
struct A { };

template<class UC, class TC,
	 template<auto...> class TTC, TC... VC>
struct C : A<TC, TTC, VC...> { };
