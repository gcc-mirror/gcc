/* { dg-options "-fexceptions" } */
/* { dg-require-effective-target exceptions } */

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

