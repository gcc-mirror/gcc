/* PR/13994; bug_cond2 was rejected on gcc up to version 3.4.x */
/* { dg-do compile } */
/* { dg-options "-std=gnu89" } */

struct s { char c[1]; };
struct s a;

int bug_if(void) { if (a.c) return 1; else return 0; }
int bug_while(void) { while (a.c); }
int bug_do_while(void) { do ; while (a.c); }
int bug_for(void) { for ( ; a.c; ) ; }
int bug_or(void) { return (a.c || 1); }
int bug_and(void) { return (a.c && 1); }
int bug_cond(void) { return (a.c ? 1 : 0); }
char *bug_cond2(void) { return (a.c ? : 0); }
int bug_not(void) { return !a.c; }
int bug_bool(void) { return (_Bool) a.c; }
