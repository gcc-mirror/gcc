/* PR c++/64767 */
/* { dg-do compile } */
/* { dg-options "-Wpointer-compare" } */
/* { dg-additional-options "-std=c++03" { target c++ } } */

int
f1 (int *p, int **q)
{
  int r = 0;

  r += p == '\0'; /* { dg-warning "10:comparison between pointer and zero character" } */
  r += p == L'\0'; /* { dg-warning "10:comparison between pointer and zero character" } */
  r += p != '\0'; /* { dg-warning "10:comparison between pointer and zero character" } */
  r += p != L'\0'; /* { dg-warning "10:comparison between pointer and zero character" } */

  r += '\0' == p; /* { dg-warning "13:comparison between pointer and zero character" } */
  r += L'\0' == p; /* { dg-warning "14:comparison between pointer and zero character" } */
  r += '\0' != p; /* { dg-warning "13:comparison between pointer and zero character" } */
  r += L'\0' != p; /* { dg-warning "14:comparison between pointer and zero character" } */

  r += q == '\0'; /* { dg-warning "10:comparison between pointer and zero character" } */
  r += q == L'\0'; /* { dg-warning "10:comparison between pointer and zero character" } */
  r += q != '\0'; /* { dg-warning "10:comparison between pointer and zero character" } */
  r += q != L'\0'; /* { dg-warning "10:comparison between pointer and zero character" } */

  r += '\0' == q; /* { dg-warning "13:comparison between pointer and zero character" } */
  r += L'\0' == q; /* { dg-warning "14:comparison between pointer and zero character" } */
  r += '\0' != q; /* { dg-warning "13:comparison between pointer and zero character" } */
  r += L'\0' != q; /* { dg-warning "14:comparison between pointer and zero character" } */

  return r;
}

int
f2 (int *p)
{
  int r = 0;

  /* Keep quiet.  */
  r += p == (void *) 0;
  r += p != (void *) 0;
  r += (void *) 0 == p;
  r += (void *) 0 != p;

  r += p == 0;
  r += p != 0;
  r += 0 == p;
  r += 0 != p;

  return r;
}

int
f3 (int *p)
{
  int r = 0;

  r += p == (char) 0; /* { dg-warning "10:comparison between pointer and zero character" } */
  r += p != (char) 0; /* { dg-warning "10:comparison between pointer and zero character" } */

  r += (char) 0 == p; /* { dg-warning "17:comparison between pointer and zero character" } */
  r += (char) 0 != p; /* { dg-warning "17:comparison between pointer and zero character" } */

  return r;
}
