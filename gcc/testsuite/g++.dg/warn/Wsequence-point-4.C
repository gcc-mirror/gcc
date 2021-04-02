// PR c++/98126
// { dg-do compile }
// { dg-options "-Wsequence-point" }
// Make sure we don't hang when verify_tree processes a large expression.

struct T { bool operator==(const T &ot) const; };

#define CMP(M, N, L) t[100 * M + 10 * N + L] == ot.t[100 * M + 10 * N + L] &&

#define CMP1(M, N) \
  CMP(M, N, 0) \
  CMP(M, N, 1) \
  CMP(M, N, 2) \
  CMP(M, N, 3) \
  CMP(M, N, 4) \
  CMP(M, N, 5) \
  CMP(M, N, 6) \
  CMP(M, N, 7) \
  CMP(M, N, 8) \
  CMP(M, N, 9)

#define CMP2(M) \
  CMP1(M, 0) \
  CMP1(M, 1) \
  CMP1(M, 2) \
  CMP1(M, 3) \
  CMP1(M, 4) \
  CMP1(M, 5) \
  CMP1(M, 6) \
  CMP1(M, 7) \
  CMP1(M, 8) \
  CMP1(M, 9)

#define GENERATE_CMPS \
  CMP2(0) \
  CMP2(1) \
  CMP2(2) \
  CMP2(3) \
  CMP2(4) \
  CMP2(5) \
  CMP2(6) \
  CMP2(7) \
  CMP2(8) \
  CMP2(9)

struct C {
  bool operator==(const C &ot) const {
    return
      GENERATE_CMPS
      true;
  }
  T t[999];
};
