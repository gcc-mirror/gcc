// PR c++/19457

int i=-1;
unsigned int j= ~0; // { dg-bogus "" }
