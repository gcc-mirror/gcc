/* { dg-do compile } */
/* { dg-options "-fsanitize=thread -fnon-call-exceptions -fexceptions" } */

typedef __complex__ float Value;
typedef struct {
  Value a[16 / sizeof (Value)];
} A;

A sum(A a,A b)
{
  a.a[0]+=b.a[0];
  a.a[1]+=b.a[1];
  return a;
}
