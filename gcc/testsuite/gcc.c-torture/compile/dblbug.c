union real_extract
{
  double  d;
  int i[sizeof (double ) / sizeof (int)];
};

typedef struct
{
  int zzzz;
} *rtx;

rtx
immed_real_const_1 (d)
     double  d;
{
  union real_extract u;
  register rtx r;

  u.d = d;
  foo (&(r->zzzz), &u);
}
