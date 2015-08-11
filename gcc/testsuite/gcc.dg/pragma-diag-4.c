/* { dg-do compile } */
/* { dg-options "-Wsign-compare -Werror=sign-compare -Werror=switch-enum" } */
/* { dg-message "warnings being treated as errors" "" {target "*-*-*"} 0 } */

int bar()
{
  unsigned x = 0;
  int y = 1;

  /* generates an error - ok */
  x += x < y ? 1 : 0; /* { dg-error "comparison" } */

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wsign-compare"
  /* generates no diagnostic - ok */
  x += x < y ? 1 : 0;
#pragma GCC diagnostic pop

  x += x < y ? 1 : 0; /* { dg-error "comparison" } */

  return x;
}

enum EE { ONE, TWO };

int f (enum EE e)
{
  int r = 0;

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wswitch-enum"

  switch (e)
    {
    case ONE:
      r = 1;
      break;
    }
#pragma GCC diagnostic pop

  switch (e) /* { dg-error "switch" } */
    {
    case ONE:
      r = 1;
      break;
    }
  return r;
}
