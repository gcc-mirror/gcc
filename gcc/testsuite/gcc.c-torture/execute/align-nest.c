
void foo(int n)
{
  typedef struct
  {
    int value;
  } myint;
  
  struct S
  {
    int i[n];
    unsigned int b:1;
    myint mi;
  } __attribute__ ((packed)) __attribute__ ((aligned (4)));

  struct S s[2];
  int k;
  
  for (k = 0; k < 2; k ++)
    s[k].mi.value = 0;
}

int main ()
{
  foo (2);
  return 0;
}

