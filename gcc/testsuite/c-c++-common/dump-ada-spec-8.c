/* { dg-do compile } */
/* { dg-options "-fdump-ada-spec -w" } */

typedef struct T {} My_T;

int foo (My_T *t);


struct S1;

struct S2 { struct S1 *s; };

struct S1 {};

/* { dg-final { scan-ada-spec-not "System.Address" } } */
/* { dg-final { cleanup-ada-spec } } */
