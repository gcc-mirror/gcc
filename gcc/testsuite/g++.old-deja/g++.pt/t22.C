// Build don't link: 

class AA { public: static int xx; };
template <class X> class A {
 public:
  static int x;
};

int AA::xx;
template <class Y> int A<Y>::x;	// gets bogus error
