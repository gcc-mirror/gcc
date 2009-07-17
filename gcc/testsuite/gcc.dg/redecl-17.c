/* { dg-do compile } */

void (*fp)(const int i);
void (*fp)(int i);

void foo()
{
  (*fp)(0);
}

