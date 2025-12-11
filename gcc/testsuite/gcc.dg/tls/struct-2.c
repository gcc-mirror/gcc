/* { dg-do assemble } */
/* { dg-require-effective-target tls } */
/* { dg-add-options tls } */

struct pixel
{
  unsigned int r, g, b;
};

struct line
{
  unsigned int length;
  struct pixel data[16];
};

__thread struct line L;

unsigned int read_r (unsigned int i)
{
  return i < L.length ? L.data[i].r : 0;
}
