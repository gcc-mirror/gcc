void abort (void);
void exit (int);

void foo (unsigned int * p)
{
  if ((signed char)(*p & 0xFF) == 17 || (signed char)(*p & 0xFF) == 18)
    return;
  else
    abort ();
}

int main ()
{
  int i = 0x30011;
  foo(&i);
  exit (0);
}
