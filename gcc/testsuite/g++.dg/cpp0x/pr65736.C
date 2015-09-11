// PR c++/65736
// { dg-do compile { target c++11 } }

int a[1];                                                                                                                                 
char *b[1] { (char *)&a[0] + 1 };
