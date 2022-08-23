// PR c++/96637
// { dg-do compile }

void foo(int[] alignas[1] alignas(1)){} // { dg-error "" }
