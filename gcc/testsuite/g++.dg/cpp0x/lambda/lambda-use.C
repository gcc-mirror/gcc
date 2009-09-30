// { dg-options -std=c++0x }

int main(int argc, char** argv)
{
  int i;
  int &ir = i;
  const int ci = 0;
  const int &cir = ci;

  [] { sizeof (argc); sizeof (i); sizeof (ir); sizeof (ci); sizeof (cir); };
  [] { int ia[ci]; };
}
