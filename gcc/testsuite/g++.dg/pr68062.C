/* { dg-do compile } */
/* { dg-options "-funsigned-char" } */

typedef char __attribute__ ((vector_size (4))) v4qi;
typedef unsigned char __attribute__ ((vector_size (4))) uv4qi;

v4qi v;
void ret(char a)
{
  v4qi c={a,a,a,a};
  uv4qi d={a,a,a,a};
  v = (c!=d);
}
