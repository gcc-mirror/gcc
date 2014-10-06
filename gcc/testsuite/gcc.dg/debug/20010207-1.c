/* { dg-do compile } */
/* { dg-options "-fgnu89-inline" } */
int f2 (void);
extern inline int f1 (void) {return f2();}
int f3 (void) {return f1();}
int f1 (void) {return 0;}
