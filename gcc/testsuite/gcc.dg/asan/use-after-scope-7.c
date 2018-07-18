// { dg-do run }
// { dg-additional-options "-fno-sanitize-address-use-after-scope" }

int
main (void)
{
  char *ptr;
  {
    char my_char[9];
    ptr = &my_char[0];
  }

  *ptr = 'c';
  return 0;
}
