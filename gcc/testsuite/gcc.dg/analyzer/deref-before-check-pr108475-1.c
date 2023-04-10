/* Reduced from haproxy-2.7.1: src/tcpcheck.c.  */

#define NULL ((void *)0)

int
test_1 (char **args, int cur_arg)
{
  char *p = NULL;

  if (*args[cur_arg]) {
    p = args[cur_arg];
  }

  if (p) { /* { dg-bogus "check of 'p' for NULL after already dereferencing it" } */
    return 1;
  }
  return 0;
}

int
test_2 (char **args, int cur_arg)
{
  char *p = NULL;
  char *q = NULL;

  if (*args[cur_arg]) {
    if (*args[cur_arg + 1]) {
      p = args[cur_arg];
    } else {
      q = args[cur_arg];
    }      
  }

  if (p) { /* { dg-bogus "check of 'p' for NULL after already dereferencing it" } */
    return 1;
  }
  if (q) { /* { dg-bogus "check of 'q' for NULL after already dereferencing it" } */
    return 2;
  }
  return 0;
}

int test_3 (void **pp, int flag)
{
  void *p = NULL;
  if (*pp && flag)
    p = pp;
  if (p) /* { dg-bogus "check of 'p' for NULL after already dereferencing it" } */
    return 1;
  return 0;    
}
