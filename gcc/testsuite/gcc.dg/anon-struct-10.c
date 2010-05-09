/* Test for designated initializers for anonymous structures and
   unions.  PR 10676.  */
/* { dg-do run } */
/* { dg-options "" } */

extern void abort (void);
extern void exit (int);

struct s
{
  int a;
  struct
  {
    int b;
    int c;
  };
  union
  {
    int d;
    struct
    {
      int e;
    };
  };
  struct
  {
    struct
    {
      struct
      {
	int f;
      };
    };
  };
};

struct s x =
  {
    .e = 5,
    .b = 4,
    .a = 3,
    .f = 7,
    .c = 9
  };

int
main (void)
{
  if (x.a != 3
      || x.b != 4
      || x.c != 9
      || x.d != 5
      || x.e != 5
      || x.f != 7)
    abort ();
  exit (0);
}
