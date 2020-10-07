/* { dg-do run } */
/* PR 31775 */
// { dg-additional-sources "nested-extern-2.cc" }
extern int *p_otheri;
static int i; // #1
int *p_si = &i;
int main()
{ 
  int i;
  int *p_ai = &i;
  {
    // This is an alias of #1, not a different object
    extern int i;
    int *p_ei = &i;

    *p_si = 1;
    *p_ai = 2;
    *p_ei = 3;
    if (*p_si != 3)
      return 1;
    if (*p_ai != 2)
      return 2;
    if (*p_otheri != 17)
      return 3;
  }
  return 0;
}
