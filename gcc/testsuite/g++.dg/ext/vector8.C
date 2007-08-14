/* { dg-do compile } */
/* { dg-options "" } */

/* Check for application of |, ^, and & on vector types.  */
#define vector __attribute__((vector_size(16) ))

vector float a;
vector int a1;
vector float b;
vector int b1;

int f(void)
{
 a =  a | b; /* { dg-error "" } */
 a =  a & b; /* { dg-error "" } */
 a =  a ^ b; /* { dg-error "" } */
 a1 =  a1 | b1;
 a1 =  a1 & b1;
 a1 =  a1 ^ b1;
}

