// PR c++/12613
// { dg-options "" }

enum { FOO = 1, BAR = 2 };
int a[] = { FOO: 1, BAR: 2 }; // { dg-error "" }
