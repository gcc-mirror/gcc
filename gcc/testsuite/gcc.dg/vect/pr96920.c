/* { dg-do compile } */

int a[1024];
int b[2048];

void foo (int x, int y)
{
  for (int i = 0; i < 1024; ++i)
    {
      int tem0 = b[2*i];
      int tem1 = b[2*i+1];
      for (int j = 0; j < 32; ++j)
	{
	  int tem = tem0;
	  tem0 = tem1;
	  tem1 = tem;
	  a[i] += tem0;
	}
    }
}
