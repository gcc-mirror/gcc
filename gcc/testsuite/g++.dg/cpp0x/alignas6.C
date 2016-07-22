// PR c++/69089
// { dg-do compile { target c++11 } }
// { dg-options "-Wno-attributes" }

alignas (0) int valid1;
alignas (1 - 1) int valid2;
struct Tvalid
{
  alignas (0) int i;
  alignas (2 * 0) int j;
};

alignas (-1) int invalid1; /* { dg-error "not a positive power of 2" } */
alignas (1 - 2) int invalid2; /* { dg-error "not a positive power of 2" } */
struct Tinvalid
{
  alignas (-1) int i; /* { dg-error "not a positive power of 2" } */
  alignas (2 * 0 - 1) int j; /* { dg-error "not a positive power of 2" } */
};

template <int N> struct TNvalid1 { alignas (N) int i; };
TNvalid1<0> SNvalid1;
template <int N> struct TNvalid2 { alignas (N) int i; };
TNvalid2<1 - 1> SNvalid2;

template <int N> struct TNinvalid1 { alignas (N) int i; }; /* { dg-error "not a positive power of 2" } */
TNinvalid1<-1> SNinvalid1;
template <int N> struct TNinvalid2 { alignas (N) int i; }; /* { dg-error "not a positive power of 2" } */
TNinvalid2<1 - 2> SNinvalid2;
