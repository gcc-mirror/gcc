/* { dg-do run } */

/* PR C/31499, test that the C front-end treats vectors like an array
   and that it works at runtime. */

#define vector __attribute__((__vector_size__(4*sizeof(int)) ))
vector signed int v1[]={0,1,2,3,4,5,6,7};


int main(void)
{
  int i;
  for (i = 0; i < sizeof(v1)/sizeof(v1[0]); i++)
  {
    vector int t = v1[i];
    int *d = (int*)&t;
    int j;
    for (j = 0; j < 4; j++)
      {
        if (d[j] != i * 4 + j)
	  __builtin_abort ();
      }
  }
  return 0;
}