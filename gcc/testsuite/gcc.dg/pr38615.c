/* { dg-do run } */

int t;
extern void abort (void);

int f(int t, const int *a)
{
 const int b[] = { 1, 2, 3};
 if (!t)
   return f(1, b);
 return b == a;
}

int main(void)
{
 if (f(0, 0))
   abort ();
 return 0;
}
