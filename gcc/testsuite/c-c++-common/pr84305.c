// { dg-additional-options -O3 }

int res, a, b;
void *foo;
static void f2 (int arg) { res = ((int (*)[arg][b]) foo)[0][0][0]; }
void f1 (void) { f2 (a); }
