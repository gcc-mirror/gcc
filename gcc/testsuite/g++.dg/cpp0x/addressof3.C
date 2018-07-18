// { dg-do compile }

struct S { int foo (); int s; };
int a[10];
int b;
S c;
int d = __builtin_addressof (a)[0][0];
int e = __builtin_addressof (b)[0];
int f = __builtin_addressof (c)->foo ();
