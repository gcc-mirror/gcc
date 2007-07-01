/* { dg-do compile } */
/* { dg-options "" } */

int __attribute__((vector_size (8))) v;

void foo()
{
  v += ~v;
}
