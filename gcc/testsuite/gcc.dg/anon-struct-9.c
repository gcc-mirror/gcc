/* Test for diagnostics for duplicate member names in anonymous
   structures and unions.  PR 4784.  */
/* { dg-do compile } */
/* { dg-options "" } */

struct s1
{
  int x;
  struct
  {
    int x; /* { dg-error "duplicate member" } */
  };
};

struct s2
{
  struct
  {
    int a;
    struct
    {
      int b;
    };
  };
  struct
  {
    int b; /* { dg-error "duplicate member" } */
  };
};

struct s3
{
  struct
  {
    int a;
    struct
    {
      int b;
    };
  };
  struct
  {
    int b; /* { dg-error "duplicate member" } */
    int c;
  };
};

struct s4
{
  int x;
  struct
  {
    int x;
  } y;
};

union u1
{
  int x;
  union
  {
    int x; /* { dg-error "duplicate member" } */
  };
};

union u2
{
  union
  {
    int a;
    union
    {
      int b;
    };
  };
  union
  {
    int b; /* { dg-error "duplicate member" } */
  };
};

union u3
{
  union
  {
    int a;
    union
    {
      int b;
    };
  };
  union
  {
    int b; /* { dg-error "duplicate member" } */
    int c;
  };
};

union u4
{
  int x;
  union
  {
    int x;
  } y;
};

#define D10(x) int x##0; int x##1; int x##2; int x##3; int x##4; int x##5; int x##6; int x##7; int x##8; int x##9;
#define D100(x) D10(x##0) D10(x##1) D10(x##2) D10(x##3) D10(x##4) D10(x##5) D10(x##6) D10(x##7) D10(x##8) D10(x##9)

#define S10(x) struct { D100(x##0) }; struct { D100(x##1) }; struct { D100(x##2) }; struct { D100(x##3) }; struct { D100(x##4) }; struct { D100(x##5) }; struct { D100(x##6) }; struct { D100(x##7) }; struct { D100(x##8) }; struct { D100(x##9) };

struct sbig
{
  S10(a)
  S10(b)
  S10(c)
  S10(d)
  S10(e)
  S10(f)
  S10(g)
  S10(h)
  S10(j)
  S10(k)
  struct
  {
    int a123; /* { dg-error "duplicate member" } */
  };
};
