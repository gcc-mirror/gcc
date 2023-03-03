/* { dg-do run } */
/* { dg-additional-options "-ftree-pre -ftree-partial-pre" } */

extern void exit (int);

int g;
int h;

void __attribute__((noipa)) bar ()
{
  if (g)
    exit (0);
}

int main(void)
{
  for (int i = 0; ; i++) {
      for (int j = 0; j < g; j++);
      if (i & 1) {
	  if (h)
	    continue;
	  if (g)
	    bar ();
	  g = 1;
      }
  }
}
