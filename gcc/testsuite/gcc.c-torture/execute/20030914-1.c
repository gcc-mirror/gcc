/* On IRIX 6, PB is passed partially in registers and partially on the
   stack, with an odd number of words in the register part.  Check that
   the long double stack argument (PC) is still accessed properly.  */

void abort (void);
void exit (int);

struct s { int val[16]; };

long double f (int pa, struct s pb, long double pc)
{
  int i;

  for (i = 0; i < 16; i++)
    pc += pb.val[i];
  return pc;
}

int main ()
{
  struct s x;
  int i;

  for (i = 0; i < 16; i++)
    x.val[i] = i + 1;
  if (f (1, x, 10000.0L) != 10136.0L)
    abort ();
  exit (0);
}
