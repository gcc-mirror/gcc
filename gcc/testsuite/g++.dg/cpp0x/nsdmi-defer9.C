// PR c++/126606
// { dg-do compile { target c++11 } }

struct s1 {
  struct s2
  {
    s2(int a = 0, int b = 1){}
  };
  s2 a = 1;
};
