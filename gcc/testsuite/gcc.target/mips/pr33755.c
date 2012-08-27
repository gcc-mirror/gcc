/* { dg-do link } */

volatile int gv;
const char *ptrs[2];

void
foo (volatile int *v, const char **ptrs)
{
  switch (*v & 1)
    {
    case 0:
      ptrs[0] = 0;
      break;
    case 1:
      break;
    default:
      ptrs[1] = "Some text";
      break;
    }
  while (*v > 0)
    *v -= 1;
}

int
main (void)
{
  foo (&gv, ptrs);
  return 0;
}
