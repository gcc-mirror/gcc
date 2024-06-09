void abort (void);
void exit (int);

int
ns_name_skip (unsigned char **x, unsigned char *y)
{
  *x = 0;
  return 0;
}

unsigned char a[2];

int dn_skipname(unsigned char *ptr, unsigned char *eom) {
    unsigned char *saveptr = ptr;

    if (ns_name_skip(&ptr, eom) == -1)
	        return (-1);
    return (ptr - saveptr);
}

int
main(void)
{
  if (dn_skipname (&a[0], &a[1]) == 0)
    abort ();
  exit (0);
}
