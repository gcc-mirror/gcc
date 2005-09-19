extern void abort (void);
double d = __FLT_MIN__ / 2.0;
int main()
{
  double x = __FLT_MIN__ / 2.0;
  if (x != d)
    abort ();
  return 0;
}
