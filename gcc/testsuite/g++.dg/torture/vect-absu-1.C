// { dg-do run }
// PR middle-end/111285

// The lowering of vect absu was done incorrectly

#define vect1 __attribute__((vector_size(sizeof(int))))

#define negabs(a) a < 0 ? a : -a

__attribute__((noinline))
int s(int a)
{
  return negabs(a);
}
__attribute__((noinline))
vect1 int v(vect1 int a)
{
  return negabs(a);
}

int main(void)
{
        for(int i = -10; i < 10; i++)
        {
          vect1 int t = {i};
          if (v(t)[0] != s(i))
            __builtin_abort();
        }
}
