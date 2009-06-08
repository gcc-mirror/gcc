/* { dg-do compile } */
/* { dg-options "-Wswitch" } */

enum E { A, B, C, D, E, F };

int
f1 (enum E e)
{
  switch (e)
    {
    case A: return 1;
    case B: return 2;
    case C: return 3;
    case D: return 4;
    case E: return 5;
    case F: return 6;
    case 7: return 7;	/* { dg-warning "not in enumerated type" } */
    }
  return 0;
}

int
f2 (enum E e)
{
  switch (e)
    {
    case A: return 1;
    case B: return 2;
    case C: return 3;
    case D: return 4;
    case E: return 5;
    case F: return 6;
    case 7: return 7;	/* { dg-warning "not in enumerated type" } */
    default: return 8;
    }
  return 0;
}
