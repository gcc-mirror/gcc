/* Exercise -Wframe-larger-than= with a byte-size suffix.
   { dg-do compile }
   { dg-options "-O -Wframe-larger-than=1KB" } */

extern void f (void*, ...);

void frame_size_912 (void)
{
  char a[512];
  char b[400];
  f (a, b);
}

void frame_size_1025 (void)
{
  char a[512];
  char b[513];
  f (a, b);
} /* { dg-warning "frame size of \[0-9\]+ bytes is larger than 1024 bytes" } */
