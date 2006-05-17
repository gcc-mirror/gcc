// PR c++/27491
// { dg-do compile }
// { dg-options "" }

int i = (int) { 0 };
int j = (int) { i };
int k = (int) { k };
