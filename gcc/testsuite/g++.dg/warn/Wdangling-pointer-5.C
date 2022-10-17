/* PR middle-end/104436 - spurious -Wdangling-pointer assigning local
   address to a class passed by value
   { dg-do compile }
   { dg-options "-O1 -Wall" } */

struct S
{
  S (void *p): p (p) { }
  S (const S &s): p (s.p) { }

  void *p;
};


void nowarn_assign_by_value (S s)
{
  int i;
  S t (&i);
  s = t;            // { dg-bogus "-Wdangling-pointer" }
}

void nowarn_assign_by_value_arg (S s)
{
  S t (&s);
  s = t;            // { dg-bogus "-Wdangling-pointer" }
}


void warn_assign_local_by_reference (S &s)
{
  int i;
  S t (&i);
  s = t;            // { dg-warning "-Wdangling-pointer" }
}
