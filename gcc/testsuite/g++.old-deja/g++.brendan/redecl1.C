// { dg-do assemble }
// GROUPS passed redeclaration
inline int min(int x, int y) {return x < y ? x : y;}	/* 235 */// { dg-error "" } .*
int min(int a, int b);
inline int min(int a, int b) {return (a < b)?a:b;}// { dg-error "" } .*
