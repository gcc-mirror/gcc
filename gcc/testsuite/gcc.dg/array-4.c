/* { dg-do run } */
/* { dg-options "" } */

/* Verify that GCC's extension to initialize a zero-length array
   member works properly.  */

extern void abort(void);
extern void exit(int);

struct f { int w; int x[0]; } f = { 4, { 0, 1, 2, 3 } };

int main()
{
  int i;
  for (i = 0; i < f.w; ++i)
    if (f.x[i] != i)
      abort ();
  exit(0);
}
