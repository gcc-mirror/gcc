/* { dg-do compile } */
/* { dg-options "" } */

/* Check for application of ~ on vector types.  */

#define vector __attribute__((vector_size(16) ))

vector float a;
vector int a1;

int f(void)
{
 a =  ~a; /* { dg-error "" } */
 a1 =  ~a1;
}
