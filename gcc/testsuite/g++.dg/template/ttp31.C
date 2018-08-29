// PR c++/83916
// { dg-do compile { target c++11 } }

template<class TA,
	 template<TA...> class TTA, TA... VA>
struct A { };

template<class UC, class TC,
	 template<TC...> class TTC, TC... VC>
struct C : A<TC, TTC, VC...> { };
