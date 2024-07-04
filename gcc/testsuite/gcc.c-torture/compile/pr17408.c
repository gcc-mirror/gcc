/* PRs 17408 and 17409, with different options. */
/* { dg-additional-options "-fpermissive" } */
extern int t;
extern int t = 0;
void f(){t =0;}
void g(){h(&t);}
