/* { dg-do compile } */
/* { dg-options "-fdump-ada-spec" } */

typedef struct T My_T;

int foo1 (My_T *);

int foo2 (My_T *);

/* { dg-final { scan-ada-spec-not "System.Address" } } */
/* { dg-final { cleanup-ada-spec } } */
