// PR c++/23171
// { dg-options "-O" }

int *p = (int*)(int[1]){0};
