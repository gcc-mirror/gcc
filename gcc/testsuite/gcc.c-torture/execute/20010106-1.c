/* Copyright 2001 Free Software Foundation
   Contributed by Alexandre Oliva <aoliva@redhat.com> */

int f(int i) {
  switch (i)
  {
    case -2:
      return 33;
    case -1:
      return 0;
    case 0:
      return 7;
    case 1:
      return 4;
    case 2:
      return 3;
    case 3:
      return 15;
    case 4:
     return 9;
    default:
      abort ();
  }
}

int main() {
  if (f(-1))
    abort ();
  exit (0);
}
