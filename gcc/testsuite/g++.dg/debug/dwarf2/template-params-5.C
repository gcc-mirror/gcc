// Contributed by Dodji Seketeli <dodji@redhat.com>
// Origin PR debug/30161
// { dg-options "-gdwarf-2 -dA" }
// { dg-final { scan-assembler "DW_TAG_template_type_param" } }
// { dg-final { scan-assembler "T.*DW_AT_name" } }

template <class T>
struct vector
{
  int size;

  vector () : size (0)
  {
  }
};

template<template <class T> class U>
struct bar
{
  U<int> u;
  int m;
  bar () :  m (u.size)
  {
  }
};

vector<int> v;
bar<vector> b;

