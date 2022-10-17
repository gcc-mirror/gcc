/* { dg-options "-O2 -fdump-tree-modref1"  } */
/* { dg-do compile } */
__attribute__((noinline))
void a(char *ptr, char *ptr2)
{
  (*ptr)++;
  (*ptr2)++;
}

__attribute__((noinline))
void b(char *ptr)
{
  a(ptr+1,&ptr[3]);
}

int main()
{
  char c[5]={0,1,2,0,0};
  b(c);
  return c[0]+c[4];
}
/* Check that both param offsets are determined correctly and the computation
   is optimized out.  */
/* { dg-final { scan-tree-dump "param offset:1" "modref1"  } } */
/* { dg-final { scan-tree-dump "param offset:3" "modref1"  } } */
/* { dg-final { scan-tree-dump "return 0" "modref1"  } } */
