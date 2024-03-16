void abort (void);
void exit (int);

double x = 0x1.fp1;
int main()
{
  if (x !=  3.875)
    abort ();
  exit (0);
}


