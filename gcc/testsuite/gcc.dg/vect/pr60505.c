/* { dg-do compile } */
/* { dg-additional-options "-Wall -Werror" } */

void foo(char *in, char *out, int num)
{
  int i;
  char ovec[16] = {0};

  for(i = 0; i < num ; ++i)
    out[i] = (ovec[i] = in[i]);
  out[num] = ovec[num/2];
}
