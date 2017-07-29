/* { dg-do compile } */
/* { dg-options "-fdump-ada-spec" } */

enum E1 {
    A1 = -1L,
    A2 = 0,
    A3 = 1
};

static enum {B1 = -1L, B2, B3} Obj1;

static struct { int i; } Obj2;

/* { dg-final { scan-ada-spec-not "unsigned" } } */
/* { dg-final { cleanup-ada-spec } } */
