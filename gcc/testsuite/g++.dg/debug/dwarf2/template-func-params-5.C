// Contributed by Dodji Seketeli <dodji@redhat.com>
// Origin PR debug/30161
// { dg-options "-g -dA" }
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
int
bar()
{
    U<int> u;
    return u.size;
}

vector<int> v;
int j = bar<vector>();

