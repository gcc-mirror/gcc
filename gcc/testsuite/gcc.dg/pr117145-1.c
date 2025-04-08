/* { dg-do compile } */
/* { dg-options "-std=c23" } */

void b();
int e(int c, struct d { [[gnu::vector_size(4)]] char an[c]; } *)
{
   (void)sizeof(struct d);
   return 0;
}
void f() {
  if (e(0, 0))
    b();
}

