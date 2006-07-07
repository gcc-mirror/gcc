/* { dg-do compile } */

int __attribute__((vector_size(8))) a;

void foo()
{
    a += a*a;
}
