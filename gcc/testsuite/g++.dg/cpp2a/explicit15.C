// PR c++/89836
// { dg-do compile { target c++20 } }

struct W { 
  constexpr explicit operator bool() { return true; };
};

struct U {
  explicit(W()) U(int);
};
