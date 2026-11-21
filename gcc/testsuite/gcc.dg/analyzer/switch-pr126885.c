/* Regression test for buffer over-read false positive in "CSWTCH"
   (PR analyzer/126885).  */
   
/* { dg-additional-options "-O2" } */

static int
optc_to_fileno (int c)
{
  int ret = -1;

  switch (c) /* { dg-bogus "-Wanalyzer-out-of-bounds" "PR analyzer/126885" } */
    {
    case 'e':
      ret = 2;
      break;
    case 'i':
      ret = 0;
      break;
    case 'o':
      ret = 1;
      break;
    }

  return ret;
}
int
main (int argc, char *argv)
{
  char c = argv[argc - 1];
  switch (c)
    {
    case 'e':
    case 'i':
    case 'o':
      {
        int value = optc_to_fileno (c);
        return value;
      }
    }
}
