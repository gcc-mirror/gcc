// GROUPS passed conversions
// cvt file
// Message-Id: <9301071708.AA03432@muresh.et.tudelft.nl>
// From: stravers@muresh.et.tudelft.nl (Paul Stravers)
// Subject: conversion method never called
// Date: Thu, 7 Jan 93 18:08:33 +0100

#include <stdio.h>

class test
{
   double d;
   int    i;
public:
   test(double dd,int ii) {d=dd; i=ii;} // constructor
   operator int&()        {return i;} // define a conversion from test to int&
   int& geti()            {return i;} // same thing, but different
};

int main()
{
   test t(3.14, 5);  // Create an object t of class "test"
   int x = (int&)t;  // This should call operator int&() but it does not ...
   int y = t.geti(); // x and y should both be 5 ...
   if (x == 5 && y == 5)
     printf ("PASS\n");
   else
     printf ("FAIL\n");
}
