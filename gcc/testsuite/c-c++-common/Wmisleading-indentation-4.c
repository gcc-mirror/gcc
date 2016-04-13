/* PR c++/70639 */
/* { dg-do compile } */
/* { dg-options "-Wmisleading-indentation" } */

void bar (int);
void
foo (int x)
{
  switch (x);
	bar (x);
}
