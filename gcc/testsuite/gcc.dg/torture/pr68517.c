/* { dg-do compile } */

typedef struct
{
} st1;

typedef struct
{
  volatile int c;
} __attribute__ ((aligned (4))) st2;

struct s4
{
  st1 f1;
  st2 f2;
  st1 f3;
};

struct s3;

void
foo (struct s3 *arg, struct s4 *arg1)
{
  arg1->f1 = (st1) { };
  arg1->f3 = (st1) { };
}
