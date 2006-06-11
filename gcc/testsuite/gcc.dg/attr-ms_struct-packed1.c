/* Test for MS structure with packed attribute.  */
/* { dg-do run { target *-*-interix* *-*-mingw* *-*-cygwin* i?86-*-darwin* } }
/* { dg-options "-std=gnu99" } */

extern void abort ();

union u
{
  int a;
} __attribute__((__ms_struct__, __packed__));

struct s
{
  char c;
  union u u;
};

int
main (void)
{
  if (sizeof (struct s) != (sizeof (char) + sizeof (union u))) 
    abort ();

  return 0;
}
