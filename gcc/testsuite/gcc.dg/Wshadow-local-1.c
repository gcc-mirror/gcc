/* { dg-do compile } */
/* { dg-options "-Wshadow=local" } */

int decl1;			/* should not warn */
void foo (double decl1)		/* should not warn */
{				
}

void foo2 (int d)		/* { dg-message "shadowed declaration" } */
{
  {
    double d;			/* { dg-warning "shadows a parameter" } */
  }
}

void foo3 ()
{
  int local;			/* { dg-message "shadowed declaration" } */
  {
    int local;			/* { dg-warning "shadows a previous local" } */
  }
}
