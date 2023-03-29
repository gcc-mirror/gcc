/* { dg-do compile }
   { dg-options "-O1 -Wall -Wno-class-memaccess -Wno-dangling-reference" } */

struct A { A (); };

const A& return_arg (const A &a)
{
  return a;
}

void sink (const void*);

void nowarn_ref (int i)
{
  const A &a = return_arg (A ()); // { dg-note "unnamed temporary" }
  sink (&a);                      // { dg-warning "-Wdangling-pointer" }
}

void warn_dangling_ref (int i)
{
  const A &a = return_arg (A ()); // { dg-note "unnamed temporary" }
  sink (&a);                      // { dg-warning "-Wdangling-pointer" }
}
