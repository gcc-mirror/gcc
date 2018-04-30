/* { dg-do compile } */
/* { dg-options "-O2 -std=c++11" } */

class a
{
public:
  int c (const char *);
};
class B
{
  virtual int *d (a, bool);
};

bool e, f, g;

class: B
{
  int ah;
  int *
  d (a, bool)
  {
    if (e)
      return &ah;
    a bj;
    bj.c ("");
    if (f)
      return &ah;
    bj.c ("");
    if (f)
      return &ah;
    bj.c ("");
    if (f)
      return &ah;
    bj.c ("");
    if (f)
      return &ah;
    bj.c ("");
    if (f)
      return &ah;
    bj.c ("");
    if (f)
      return &ah;
    bj.c ("");
    if (f)
      return &ah;
    bj.c ("");
    if (f)
      return &ah;
    bj.c ("");
    if (f)
      return &ah;
    bj.c ("");
    if (f)
      return &ah;
    bj.c ("");
    if (f)
      return &ah;
    bj.c ("");
    if (f)
      return &ah;
    bj.c ("");
    if (f)
      return &ah;
    bj.c ("");
    if (f)
      return &ah;
    bj.c ("");
    if (f)
      return &ah;
    bj.c ("");
    if (f)
      return &ah;
    bj.c ("");
    if (f)
      return &ah;
    bj.c ("");
    if (f)
      return &ah;
    bj.c ("");
    if (f)
      return &ah;
    bj.c ("");
    if (f)
      return &ah;
    bj.c ("");
    if (f)
      return &ah;
    bj.c ("");
    if (f)
      return &ah;
    bj.c ("");
    if (f)
      return &ah;
    bj.c ("");
    if (f)
      return &ah;
    bj.c ("");
    if (f)
      return &ah;
    bj.c ("");
    if (f)
      return &ah;
    bj.c ("");
    if (f)
      return &ah;
    bj.c ("");
    if (g)
      return &ah;
    if (f)
      return &ah;
      e = a ().c("");
    return &ah;
  }
} b;

