// { dg-do compile }
// PR c++/17661
// We used to try to create a temprary for the condition
// expression in the for which was wrong.


struct C
{
  C (const C &x);
};
C &f();
void breakme (C j, bool k)
{
  for (;; k ? j : f())  ;
}
