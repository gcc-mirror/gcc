/* { dg-options "-O2 -fdump-tree-switchconv" } */
/* { dg-do run } */

extern void abort (void);

static int X, Y;

int check(int param)
{
  int a = 0;
  int b = 1;
  
  switch (param) 
    {
    case -2:
      a = 0;
      b = -1;
      break;
    case 1:
    case 2:
      a = 8;
      b = 6;
      break;
    case 3:
      a = 9;
      b = 5;
      break;
    case 6:
      a = 10;
      b = 4;
      break;
    default:
      a = 16;
      b = 1;
    }
  
  X = a;
  Y = b;
  return 0;
}

void assertions(int a, int b)
{
  if (X != a || Y != b)
    abort();  

  return;
}

int main ()
{
  check (-10);
  assertions (16, 1);

  check (-2);
  assertions (0, -1);

  check(1);
  assertions (8, 6);

  check(2);
  assertions (8, 6);

  check(3);
  assertions (9, 5);

  check(5);
  assertions (16, 1);

  check(6);
  assertions (10, 4);

  check(12);
  assertions (16, 1);

  return 0;
}

/* { dg-final { scan-tree-dump "Switch converted" "switchconv" } } */
