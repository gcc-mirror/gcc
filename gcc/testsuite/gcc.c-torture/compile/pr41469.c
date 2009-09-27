/* { dg-options "-fexceptions" } */

void
af (void *a)
{
}
void
bf (void)
{
  int i = 1;
  char v[i];
  af (v);
}

