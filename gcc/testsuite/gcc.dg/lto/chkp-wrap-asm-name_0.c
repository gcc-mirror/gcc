/* { dg-lto-do link } */
/* { dg-require-effective-target mpx } */
/* { dg-lto-options { { -O2 -flto -fcheck-pointer-bounds -mmpx } } } */

typedef long unsigned int size_t;

extern size_t strlen (const char *);
extern __typeof (strlen) strlen __asm__ ("" "__hidden_strlen") __attribute__ ((visibility ("hidden")));

size_t
test1 (const char *p) { return strlen (p); }

size_t
test2 (const char *p) { return __builtin_strlen (p); }

int
main (int argc, const char **argv)
{
  return test1 (argv[0]) - test2 (argv[0]);
}
