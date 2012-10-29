
void exit (int);
void abort (void);
int a[1];
void (*terminate_me)(int);

__attribute__((noinline,noclone))
t(int c)
{ int i;
  for (i=0;i<c;i++)
    {
      if (i)
       terminate_me(0);
      a[i]=0;
    }
}
main()
{
  terminate_me = exit;
  t(100);
  abort();
}
