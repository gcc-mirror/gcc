/* PR/11658  The error message was quite mysterious for gcc up to 3.4.x */

struct a {
  int x;
};

int bug_if(struct a a) { if (a) return 1; else return 0; }  /* { dg-error "struct type" } */
int bug_while(struct a a) { while (a); }  /* { dg-error "struct type" } */
int bug_do_while(struct a a) { do ; while (a); }  /* { dg-error "struct type" } */
int bug_for(struct a a) { for ( ; a; ) ; }  /* { dg-error "struct type" } */
int bug_or(struct a a) { return (a || 1); }  /* { dg-error "struct type" } */
int bug_and(struct a a) { return (a && 1); }  /* { dg-error "struct type" } */
int bug_cond(struct a a) { return (a ? 1 : 0); }  /* { dg-error "struct type" } */
int bug_cond2(struct a a) { return (a ? : 0); }  /* { dg-error "struct type" } */
int bug_bool(struct a a) { return (_Bool) a; }  /* { dg-error "struct type" } */
