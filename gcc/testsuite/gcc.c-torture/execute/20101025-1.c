void abort (void);
void exit (int);

static int g_7;
static int *volatile g_6 = &g_7;
int g_3;

static int f1 (int *p_58)
{
    return *p_58;
}

void f2 (int i) __attribute__ ((noinline));
void f2 (int i)
{
  g_3 = i;
}

int f3 (void) __attribute__ ((noinline));
int f3 (void)
{
    *g_6 = 1;
    f2 (f1 (&g_7));
    return 0;
}

int main ()
{
  f3 ();
  if (g_3 != 1)
    abort ();
  exit (0);
}
