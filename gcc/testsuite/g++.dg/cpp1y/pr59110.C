// PR c++/59110
// { dg-options "-std=c++1y" }

int i = *(auto*)0; // { dg-error "" }
