/* This does not work on NetWare, which has a default of 1-byte alignment.  */
/* { dg-xfail-if "" { "*-*-netware*" } { "*" } { "" } } */
typedef struct _foo foo;
extern foo bar;
struct _foo {
  int a;
};

int tst[__alignof__ (bar) >= __alignof__ (int) ? 1 : -1];
