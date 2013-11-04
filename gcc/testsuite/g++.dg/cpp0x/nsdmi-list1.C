// PR c++/50563
// { dg-options -std=c++11 }

struct S1 {
  int a{10}, b{20};     // OK
};

struct S2 {
  int a, b = 20;        // OK
};

struct S3 {
  int a = 10, b = 20;
};
