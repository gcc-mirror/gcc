// { dg-do run }

int i;

#define CV(X) (__builtin_constant_p (X) ? (X) : -1)

int ar[] = { CV (i == 42 && false) };

int main()
{
  if (ar[0] != 0)
    __builtin_abort();
}
