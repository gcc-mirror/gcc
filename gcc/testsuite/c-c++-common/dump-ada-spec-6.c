/* { dg-do compile } */
/* { dg-options "-fdump-ada-spec" } */

typedef enum {
    Zero
} MyEnum;

typedef MyEnum SomethingElse; 

/* { dg-final { cleanup-ada-spec } } */
