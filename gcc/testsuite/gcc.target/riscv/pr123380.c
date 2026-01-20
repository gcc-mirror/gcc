/* { dg-do compile } */
/* { dg-options "-march=rv64gc -mabi=lp64d -O2" { target { rv64 } } } */
/* { dg-options "-march=rv32gc -mabi=ilp32d -O2" { target { rv32 } } } */

void *p;
int d;
char c;

void
foo (_Float16 *fp)
{
  _Float16 f = *fp;
  do {
    __builtin_strcat (p, 0);
    __builtin_memmove (1 + (char *) &f, &f, 1);
  } while (d);
  c = f;
}
