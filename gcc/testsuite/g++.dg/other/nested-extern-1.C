/* { dg-do run } */
// { dg-additional-options "-fpermissive" }
// { dg-additional-sources "nested-extern-1.cc" }
/* PR 31775 */
extern int *p_otheri;
extern int *p;
int main()
{ 
  extern int i; // { dg-message "previous declaration" }
  i = 1;
  *p = 2;
  if (i != 2)
    return 1;
  if (p_otheri != p)
    return 2;
  return 0;
}

// This is extern because of the injection above.
static int i; // { dg-warning ".extern. and later .static" }
int *p = &i;
