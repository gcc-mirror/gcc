/* PR c++/35652: wrong location and duplicated warning.
 { dg-do compile }
 { dg-options "" } */
int bar()
{
  const char *s = 'z' + "y"; /* { dg-warning "offset '122' outside bounds of constant string" } */
}

int g()
{
  char str[2];
  const char *p = str + sizeof(str);
}
