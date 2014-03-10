// { dg-do compile { target c++11 } }

int main(int argc, char** argv)
{
  int i;
  int &ir = i;
  const int ci = 1;
  const int &cir = ci;

  [] { sizeof (argc); sizeof (i); sizeof (ir); sizeof (ci); sizeof (cir); };
  [] { int ia[ci]; };
}
