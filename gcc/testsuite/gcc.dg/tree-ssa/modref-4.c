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
  a(ptr+1,&ptr[2]);
}

int main()
{
  char c[3]={0,1,0};
  b(c);
  return c[0]+c[2];
}
/* Check that both param offsets are determined correctly and the computation
   is optimized out.  */
/* { dg-final { scan-tree-dump "param offset:1" "modref1"  } } */
/* { dg-final { scan-tree-dump "param offset:2" "modref1"  } } */
/* { dg-final { scan-tree-dump "return 0" "modref1"  } } */
