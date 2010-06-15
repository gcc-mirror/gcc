/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-esra-details" } */

struct blah
{
  char a[4];
};

struct str
{
  struct blah b1;
  char x;
};

struct val
{
  char y;
  struct blah b2;
};

union U
{
  struct str str;
  struct val val;
};


extern struct blah e_b1, e_b2;
extern union U *e_u;

int foo (int b)
{
  union U u;

  u.str.b1 = e_b1;
  u.val.b2 = e_b2;
  u.str.b1.a[3] = 0;

  *e_u = u;
}

/* { dg-final { scan-tree-dump-times "Created a replacement" 0 "esra"} } */
/* { dg-final { cleanup-tree-dump "esra" } } */
