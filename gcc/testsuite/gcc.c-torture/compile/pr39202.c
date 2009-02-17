typedef struct
{
  union
    {
      int * aaa;
    } u;
} t_a;

typedef struct
{
  unsigned bbb : 1;
} t_b;

typedef struct
{
  int ccc;
  t_a ddd;
  t_b eee;
  int fff;
} t_c;

typedef struct t_d
{
  t_c f1;
  t_c f2;
} t_d;

void foo (void)
{
  t_d ggg;
  ggg.f1 = ggg.f2;
}

