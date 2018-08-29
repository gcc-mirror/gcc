/* { dg-do compile } */
/* { dg-options "-fdump-ada-spec" } */

typedef struct T *My_Ptr1;

int foo1 (My_Ptr1);

typedef struct T *My_Ptr2;

int foo2 (My_Ptr2);

struct T { int i; };

/* { dg-final { scan-ada-spec-not "System.Address" } } */
/* { dg-final { cleanup-ada-spec } } */
