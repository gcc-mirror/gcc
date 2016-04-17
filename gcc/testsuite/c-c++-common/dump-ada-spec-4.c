/* { dg-do compile } */
/* { dg-options "-fdump-ada-spec" } */

typedef struct Message_Type
{
   int a;
} Message_Type;

extern int Func(const Message_Type *in, Message_Type *out);

/* { dg-final { scan-ada-spec-not "System.Address" } } */
/* { dg-final { cleanup-ada-spec } } */
