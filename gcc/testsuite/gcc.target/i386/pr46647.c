/* { dg-do compile } */
/* { dg-options "-O2 -mtune=generic" } */

char a[5];
int
func1 (void)
{
  __builtin_memset (a,-1,sizeof (a));
  return 0;
}

int a2[5];
int
func2 (void)
{
  __builtin_memset (a2,-1,sizeof (a2));
  return 0;
}

char a3[5];
int
func3 (void)
{
  __builtin_memset (a3,0x8fffffff,sizeof (a3));
  return 0;
}

char a4[5];
int
func4 (void)
{
  __builtin_memset (a4,0x8fffff00,sizeof (a4));
  return 0;
}

int a5[5];
int
func5 (void)
{
  __builtin_memset (a5,0x8fffffff,sizeof (a5));
  return 0;
}

/* { dg-final { scan-assembler-not "call\[\\t \]*_?memset" } } */
