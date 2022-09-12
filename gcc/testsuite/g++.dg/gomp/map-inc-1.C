int main (int argc, char *argv[])
{
  int a = 5;
#pragma omp target map(++a)
  /* { dg-message {sorry, unimplemented: unsupported map expression '\+\+ a'} "" { target *-*-* } .-1 } */
  {
    a++;
  }
  return 0;
}
