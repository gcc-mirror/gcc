// 68689 - flexible array members in unions accepted in C++
// { dg-do compile }
// { dg-options "-Wno-error=pedantic" }

union U_i_ax {
    int i;
    int a[];
};

struct SU1 {
  union {
    int a[];
  };
};

struct SU2 {
  int n;
  union {
    int a[];
  };
};

struct SU3 {
  union {
    int n;
    int a[];
  };
};

union U_i_a0 {
    int i;
    int a[0];
};
