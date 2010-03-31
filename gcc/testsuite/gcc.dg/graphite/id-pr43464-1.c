typedef struct regnode
{
  char flags;
} regnode;
extern const unsigned char A[];

char *foo (regnode *c, char *s, int norun)
{
  int uskip;
  while (s + (uskip = A[*s]))
    {
      if ((c->flags || bar (c)) && norun)
	goto got_it;
      s += uskip;
    }
 got_it:
  return s;
}
