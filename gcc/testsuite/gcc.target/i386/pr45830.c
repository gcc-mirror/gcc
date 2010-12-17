/* PR target/45830 */
/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-switchconv-all -mtune=generic" } */

int
foo (int *a)
{
  switch (*a)
    {
    case 0:
    case 3:
    case 1:
    case 2:
    case 4:
    case 23:
    case 26:
    case 19:
    case 5:
    case 21:
    case 20:
    case 22:
    case 27:
      return 1;
    default:
      return 0;
    }
}

/* { dg-final { scan-tree-dump "Expanding as bit test is preferable" "switchconv" } } */
/* { dg-final { scan-assembler-not "CSWTCH" } } */
/* { dg-final { cleanup-tree-dump "switchconv" } } */
