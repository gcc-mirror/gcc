// GROUPS passed miscellaneous
extern "C" int printf (const char *, ...);

int main()
{
  int i = 0;
  // Make sure build_unary_op correctly computes this.
  int *pi = &(++i);
  *pi = 4;

  if (i != 4)
    printf ("FAIL\n");
  else
    printf ("PASS\n");
}
