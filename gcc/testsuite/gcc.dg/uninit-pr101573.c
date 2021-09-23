/* { dg-do compile } */
/* { dg-options "-O0 -Wuninitialized" } */

int main(int argc, char **argv)
{
  int a;
  for(; a < 5; ++a) /* { dg-warning "is used uninitialized" } */
    ;
  return  0;
}
