// PR c++/104403
// { dg-do compile { target c++11 } }

int
main ()
{
  []{ switch (0) { case 0: static int value; return &value; } };
}
