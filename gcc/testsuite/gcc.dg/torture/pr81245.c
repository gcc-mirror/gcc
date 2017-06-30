/* { dg-options "-ffast-math" } */
/* { dg-do compile } */
/* This test used to crash the vectorizer as the ifconvert pass
   used to convert the if to copysign but called update_stmt on
   the old statement after calling fold_stmt. */
double sg[18];
void f(void)
{
  for (int i = 0 ;i < 18;i++)
  {
    if (sg[i] < 0.0)
      sg[i] = -1.0;
    else
      sg[i] = 1.0;
  }
}
