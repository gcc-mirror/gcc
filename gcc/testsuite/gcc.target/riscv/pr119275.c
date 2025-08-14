/* { dg-do compile { target int128 } } */
/* { dg-options "-march=rv64gv -mabi=lp64d -mrvv-vector-bits=zvl" { target { rv64 } } } */

__int128 h, j;
int y;
double d;
void *p;
char *q;
char x;
long u;

char *bar(int, int);

typedef __attribute__((__vector_size__ (2))) char V;

void
foo(V v)
{
  x += *bar (0, 0);
  for(;;) {
    __builtin_strcat (p, 7 + q);
    d += __builtin_stdc_rotate_left (
        (unsigned __int128) u | h << *__builtin_strcat (p, 7 + q), j);
    u += (long) __builtin_memmove (&y, &v, 2);
  }
}
