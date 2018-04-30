/* { dg-do compile } */

typedef int h;
typedef int k;
int a;
int b;
int c;
int d;
int e;
int f(int g)
{
  h *i = &e;
  k *j;
  if (d -= b)
    for (; *j; *j += 1) {
	g = g || (a = e ? c = (__UINTPTR_TYPE__)j : 0) + *i;
	i = &d;
    }
}
