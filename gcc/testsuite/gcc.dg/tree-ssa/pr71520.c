/* PR tree-optimization/71520 */
/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

void bar (int);

void
foo (int x)
{
  switch (x)
    {
    case 1:
    case 12:
    case 28:
    case 174:
      bar (1);
      bar (2);
      break;
    case 3:
    case 7:
    case 78:
    case 96:
    case 121:
    default:
      bar (3);
      bar (4);
      bar (5);
      bar (6);
      break;
    case 8:
    case 13:
    case 27:
    case 19:
    case 118:
      bar (3);
      bar (4);
      bar (5);
      bar (6);
      break;
    case 4:
      bar (7);
      break;
    }
}

void
baz (int x)
{
  switch (x)
    {
    case 1:
    case 12:
    case 28:
    case 174:
      bar (8);
      bar (9);
      break;
    case 3:
    case 7:
    case 78:
    case 96:
    case 121:
    default:
    lab1:
    lab2:
      bar (10);
      bar (11);
      bar (12);
      bar (13);
      break;
    case 8:
    case 13:
    case 27:
    case 19:
    case 118:
    lab3:
    lab4:
      bar (10);
      bar (11);
      bar (12);
      bar (13);
      break;
    case 4:
      bar (14);
      break;
    }
}

/* { dg-final { scan-tree-dump-times "bar \\\(3\\\);" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "bar \\\(10\\\);" 1 "optimized" } } */
