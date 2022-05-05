/* PR c++/71637  */
/* { dg-options "-ftrack-macro-expansion=0 -Wmisleading-indentation" }  */

#define m(x) ({ int y; if (x) y=0; else y=1; y; })

int main()
{
  int x =
    m(0);
  return x;
}
