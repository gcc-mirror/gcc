void abort (void);
void exit (int);

typedef unsigned long long u64;
unsigned long foo = 0;
u64 f() __attribute__((noinline));

u64 f() {
  return ((u64)40) + ((u64) 24) * (int)(foo - 1);
}

int main ()
{
  if (f () != 16)
    abort ();
  exit (0);
}
