// GROUPS passed operators
// opr-conv file
// Message-Id: <199301040217.AA04377@cypress.ucsc.edu>
// From: "Dean R. E. Long" <dlong@cse.ucsc.edu>
// Subject: conversion operator bug?
// Date: Sun, 3 Jan 1993 18:17:20 -0800

#include <stdio.h>
class B {};

class A {
    B *p;
public:
    A() { p = 0; }
    operator B * () { return p; }
    operator B & () { return *p; }
};

main()
{
    A a;
    B &b = (B &)a;
    B *bp = (B *)a;
    B &br = a.operator B&();
// What's the right test?
    printf ("FAIL\n");
}
