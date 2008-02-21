/* { dg-do run } */
/* { dg-options "-O1" } */
extern void abort(void);
long long __attribute__((noinline)) get(void)
{
  return -2;
}
long long __attribute__((noinline)) get(void);
int __attribute__((noinline)) check(void)
{
 long long lcn;

 lcn = get();
 if (lcn >= 0 || lcn == -1)
  return 0;

 return -1;
}
int main()
{
  if (check() == 0)
    abort();
  return 0;
}
