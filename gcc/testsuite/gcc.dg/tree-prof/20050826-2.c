/* Testcase derived from gcc.c-torture/execute 20050826-2.c
   which showed jump threading profile insanities.  */
/* { dg-options "-Ofast -fdump-tree-dom2-all" } */

struct rtattr
{
  unsigned short rta_len;
  unsigned short rta_type;
};

__attribute__ ((noinline))
int inet_check_attr (void *r, struct rtattr **rta)
{
  int i;

  for (i = 1; i <= 14; i++)
    {
      struct rtattr *attr = rta[i - 1];
      if (attr)
	{
	  if (attr->rta_len - sizeof (struct rtattr) < 4)
	    return -22;
	  if (i != 9 && i != 8)
	    rta[i - 1] = attr + 1;
	}
    }
  return 0;
}

extern void abort (void);

int
test (void)
{
  struct rtattr rt[2];
  struct rtattr *rta[14];
  int i;

  rt[0].rta_len = sizeof (struct rtattr) + 8;
  rt[0].rta_type = 0;
  rt[1] = rt[0];
  for (i = 0; i < 14; i++)
    rta[i] = &rt[0];
  if (inet_check_attr (0, rta) != 0)
    abort ();
  for (i = 0; i < 14; i++)
    if (rta[i] != &rt[i != 7 && i != 8])
      abort ();
  for (i = 0; i < 14; i++)
    rta[i] = &rt[0];
  rta[1] = 0;
  rt[1].rta_len -= 8;
  rta[5] = &rt[1];
  if (inet_check_attr (0, rta) != -22)
    abort ();
  for (i = 0; i < 14; i++)
    if (i == 1 && rta[i] != 0)
      abort ();
    else if (i != 1 && i <= 5 && rta[i] != &rt[1])
      abort ();
    else if (i > 5 && rta[i] != &rt[0])
      abort ();
  return 0;
}

int
main (void)
{
  int i;
  for (i=0; i<100; i++)
    test ();
  return 0;
}

/* { dg-final-use-not-autofdo { scan-tree-dump-not "Invalid sum" "dom2"} } */
