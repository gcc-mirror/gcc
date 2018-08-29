// PR c++84694 ICE on friend decl
template<typename>
struct a {
  template<int ()> void b();
  friend void b<0>(); // ICEd with useless friend decl
};
