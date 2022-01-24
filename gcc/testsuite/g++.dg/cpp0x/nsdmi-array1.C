// PR c++/103946
// { dg-do compile { target c++11 } }

struct s1 {  s1(); };
class s2 { s1 f1[2]{}; };
s2 a;
