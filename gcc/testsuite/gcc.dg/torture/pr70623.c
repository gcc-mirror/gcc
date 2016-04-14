/* { dg-do compile } */
/* { dg-additional-options "-w" } */

int nm;
int *av;

void
h9(void)
{
  for (;;) {
      int wk, rc;
      int **ptr_10 = &av;
      if (*av != 0) {
      }
u4:
      wk = 0;
      for (rc = 0; rc < 3; ++rc) {
	  int bc = (rc ? rc : nm);
	  int ud = bc ? (*av ? 0 : rc) : 1;
	  if (ud != 0) {
	      if (*av != 0)
		goto u4;
	      for (;;) {
	      }
	  }
      }
      while (wk < 3) {
	  av = **ptr_10;
	  ++wk;
      }
  }
}
