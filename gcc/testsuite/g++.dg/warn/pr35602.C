// PR 35602 Bogus warning with -Wsign-conversion
// { dg-do compile }
// { dg-options "-Wconversion -Wsign-conversion" }
struct c
{
  ~c();
  c();
};

int

main(const int,
     const char * const * const)
{
  c x[0UL][0UL] =  // { dg-bogus "warning: conversion to .long unsigned int. from .long int. may change the sign of the result" }
    {
    };
  
  c y[0UL] =
    {
    };
  
  int z[0ul][0UL] =
    {
    };
  
  return 0;
}
