// PR c++/109966
// { dg-do compile { target c++14 } }

struct k {
  k(const char *);
};
struct M {
  k name;
  int j = 42;
  int i = j;
};

M m = M{""};

struct S {
  M arr1[1]{M{""}};
  M a1[1] = { (M{""}) };
  M a2[1] = { (true, M{""}) };
  M a3[1] = { true ? M{""} : M{""} };
  M arr2[2]{M{""}, M{""}};
  M arr3[3]{M{""}, M{""}, M{""}};

  M arr1e[1] = {M{""}};
  M arr2e[2] = {M{""}, M{""}};
  M arr3e[3] = {M{""}, M{""}, M{""}};

  M arr1l[1] = { m };
  M arr2l[2] = { m, m };
  M arr3l[3] = { m, m, m };

  M m1 = M{""};
  M m2 = m;
  M m3{M{""}};
  M m4 = {M{""}};
} o;

struct N {
  N(M);
};

struct Z {
  N arr1[1]{ M{""} };
  N arr2[2]{ M{""}, M{""} };
  N arr1e[1] = { M{""} };
  N arr2e[2] = { M{""}, M{""} };
} z;

struct Y {
  k name;
  int j = 42;
  int i = j;
  operator M();
};

struct W {
  M arr1[1]{ Y{""} };
  M arr2[2]{ Y{""}, Y{""} };
  M arr3[3]{ Y{""}, Y{""}, Y{""} };
} w;
