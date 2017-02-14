/* { dg-do compile } */
/* { dg-options "-O2 -Wuninitialized" } */

int *rw;
int line_height;
int pixel_width;
int text_cols;
int width1, width2, width3;

void *pointer;

void f (int i, int j)
{
  void *ptr;
  if (i)
    {
      if (j)
	return;
      ptr = pointer;
    }
  pixel_width = 1234 + width1 + 2 * width2 + 2 * width3;
  *rw = text_cols + line_height;
  if (i)
    rw=ptr; /* { dg-bogus "uninitialized" "bogus warning" } */
}
