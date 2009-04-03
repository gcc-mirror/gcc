/* { dg-do link } */
/* { dg-options "-O -fstrict-aliasing" } */

class first
{
public:
  double d;
  int f1;
};

class middle : public first
{
};

class second : public middle
{
public:
  int f2;
  short a;
};

class third
{
public:
  char a;
  char b;
};

class multi: public third, public second
{
public:
  short s;
  char f3;
};

extern void link_error ();

void
foo (first *s1, second *s2)
{
  s1->f1 = 0;
  s2->f2 = 0;
  s1->f1++;
  s2->f2++;
  s1->f1++;
  s2->f2++;
  if (s1->f1 != 2)
    link_error ();
}

void
bar (first *s1, multi *s3)
{
  s1->f1 = 0;
  s3->f3 = 0;
  s1->f1++;
  s3->f3++;
  s1->f1++;
  s3->f3++;
  if (s1->f1 != 2)
    link_error ();
}


int
main()
{
  first a;
  second b;
  multi c;
  foo (&a, &b);
  bar (&a, &c);
  return 0;
}
