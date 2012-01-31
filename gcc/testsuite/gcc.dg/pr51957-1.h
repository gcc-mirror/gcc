union R { int c; union R *p; };
extern union R *w[];
typedef struct { int t; } T;
typedef struct { void *u; } U;
union R *fn1 (void);
void fn2 (int, const char *, union R *);
void fn3 (void);
int fn4 (union R *);
void foo (U *x);
