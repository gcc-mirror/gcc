void abort (void);
void exit (int);

static int f(int) __attribute__((const));
int main()
{
   int f1, f2, x;
   x = 1; f1 = f(x);
   x = 2; f2 = f(x);
   if (f1 != 1 || f2 != 2)
     abort ();
   exit (0);
}
static int f(int x) { return x; }
