// { dg-do compile { target c++11 } }
// { dg-options -fnew-inheriting-ctors }

struct V { V(int); };
struct W : virtual V { using V::V; };
struct X : virtual W, virtual V { using W::W; };
X x(0);
