/* Testcase for PR 40323.  */
/* { dg-do compile } */
/* { dg-options "-fno-early-inlining"  } */
/* { dg-options "-fno-early-inlining -fpie" { target { ! nonpic } } } */

extern void do_something (const char *, int);

class Parent
{
private:
  const char *data;

public:
  Parent (const char *d) : data(d)
  {}

  int funcOne (int delim) const;
};

class AnotherParent
{
private:
  double d;
public:
  AnotherParent (void) : d(0)
  {}
};


class Child : public AnotherParent, Parent
{
private:
  int zzz;
public:
  Child (const char *d) : Parent(d)
  {}
};


int Parent::funcOne (int delim) const
{
  int i;
  for (i = 0; i < delim; i++)
    do_something(data, i);

  return 1;
}

int docalling (int (Child::* f)(int delim) const)
{
  Child S ("muhehehe");

  return (S.*f)(4);
}

typedef int (Parent::* my_mp_type)(int delim);

int main (int argc, char *argv[])
{
  int i;
  int (Parent::* f)(int ) const;
  int (Child::* g)(int ) const;
  
  f = &Parent::funcOne;
  g = (int (Child::* )(int) const) f;
  i = docalling (g);
  return i;
}
