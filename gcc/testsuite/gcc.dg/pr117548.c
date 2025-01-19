/* { dg-do compile } */
/* { dg-options "-std=c23" } */

typedef struct A { int q; } A;
void f(A*);

int main(void)
{
  struct A { int q; };
  void f(struct A*);
}

