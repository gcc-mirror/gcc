/* { dg-do compile } */

struct T;
typedef void F(void);

F* aux(void (*x)())
{
  return x;
}

void make_mess (int);

F*
get_funloc (void (*x)(int), F* (*y)())
{
  return y(x);
}

F*
foo ()
{
  return get_funloc (make_mess, aux);
}
