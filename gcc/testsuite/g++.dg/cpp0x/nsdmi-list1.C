// PR c++/50563
// { dg-do compile { target c++11 } }

struct S1 {
  int a{10}, b{20};     // OK
};

struct S2 {
  int a, b = 20;        // OK
};

struct S3 {
  int a = 10, b = 20;
};
