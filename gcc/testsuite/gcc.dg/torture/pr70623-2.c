/* { dg-do compile } */

int b8, il, rc, nm;

void
h9(void)
{
  int *av = &b8;

is:
  for (;;) {
      int vj, wk;
      int *m9 = &b8;

      if (*m9 == *av) {
	  if (il == 0)
	    goto is;

di:
	  continue;
	  for (vj = 0; vj < 1; ++vj) {
	      goto di;
kz:
	      ;
	  }
      }

      for (rc = 0; rc < 2; ++rc) {
	  int bc = rc ? rc : nm;
	  int ud = bc ? (*av ? 0 : rc) : 1;

	  if (ud != 0)
	    if (*av != 0)
	      goto kz;
      }

      for (wk = 0; wk < 3; ++wk)
	++(*av);
      av = 0;
  }
}
