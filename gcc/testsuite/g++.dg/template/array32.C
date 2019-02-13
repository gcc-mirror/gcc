/* PR c++/87996 - "size of array is negative" error when SIZE_MAX/2 < sizeof(array) <= SIZE_MAX
   { dg-do compile }
   { dg-options "-ftrack-macro-expansion=0" }  */

#define SIZE_MAX   __SIZE_MAX__

typedef __SIZE_TYPE__ size_t;

template <size_t N> struct Aszmax_d2 {
  char a[N];
};
Aszmax_d2<SIZE_MAX / 2> aszmax_d2;

template <size_t N> struct Aszmax_d2_p1 {
  char a[N];   // { dg-error "exceeds maximum object size" }
};
Aszmax_d2_p1<SIZE_MAX / 2 + 1> aszmax_d2_p1;

template <size_t N> struct Aszmax {
  char a[N];   // { dg-error "exceeds maximum object size" }
};
Aszmax<SIZE_MAX> aszmax;

template <size_t M, size_t N> struct Aszmax_d2_szmax_d2 {
  char a[M][N];   // { dg-error "exceeds maximum object size" }
};
Aszmax_d2_szmax_d2<SIZE_MAX / 2, SIZE_MAX / 2> aszmaxd2_szmaxd2;
