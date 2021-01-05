// A slight variation of ttp31.C.
// { dg-do compile { target c++11 } }

template<class TA,
	 template<typename TA::type...> class TTA, TA... VA>
struct A { };

template<class UC, class TC,
	 template<typename TC::type...> class TTC, TC... VC>
struct C : A<TC, TTC, VC...> { };
