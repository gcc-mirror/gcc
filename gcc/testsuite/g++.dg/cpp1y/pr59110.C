// PR c++/59110
// { dg-do compile { target c++1y } }

int i = *(auto*)0; // { dg-error "" }
