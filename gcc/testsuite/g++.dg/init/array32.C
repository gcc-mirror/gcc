// PR c++/42315

extern int x[];

int i = x[0];

int x[] = 0;			// { dg-error "" }
