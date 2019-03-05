/* PR tree-optimization/84436 */
/* { dg-options "-O2 -fdump-tree-switchconv -fdump-tree-optimized" } */

char
lowerit(char a)
{
  switch (a)
    {
    default:
      return a;
    case 'A':
      return 'a';
    case 'B':
      return 'b';
    case 'C':
      return 'c';
    case 'D':
      return 'd';
    case 'E':
      return 'e';
    case 'F':
      return 'f';
    case 'G':
      return 'g';
    case 'H':
      return 'h';
    case 'I':
      return 'i';
    case 'J':
      return 'j';
    case 'K':
      return 'k';
    case 'L':
      return 'l';
    case 'M':
      return 'm';
    case 'N':
      return 'n';
    case 'O':
      return 'o';
    case 'P':
      return 'p';
    case 'Q':
      return 'q';
    case 'R':
      return 'r';
    case 'S':
      return 's';
    case 'T':
      return 't';
    case 'U':
      return 'u';
    case 'V':
      return 'v';
    case 'W':
      return 'w';
    case 'X':
      return 'x';
    case 'Y':
      return 'y';
    case 'Z':
      return 'z';
    }
}

/* { dg-final { scan-tree-dump-times "a_.*\\+ 32" 1 "switchconv" } } */
/* { dg-final { scan-tree-dump-not "switch" "optimized" } } */
