// { dg-lto-do link }
// { dg-require-effective-target lto_incremental }
// { dg-lto-options { { -std=c++11 -g -flto -Wno-non-c-typedef-for-linkage } } }
// { dg-extra-ld-options "-r -nostdlib" }

typedef struct {
  typedef struct { } VarSelectorRecord;
} Format14Cmap;
void fn1() { Format14Cmap a; }
