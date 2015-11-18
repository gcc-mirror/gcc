/* { dg-do compile } */
/* { dg-options "-Wswitch-enum -Wsign-compare -fstrict-overflow -Wstrict-overflow -Werror -Wno-error=switch-enum" } */
/* PR c/66098 - #pragma diagnostic 'ignored' not fully undone by pop for strict-overflow 
   PR c/66711 - GCC does not correctly restore diagnostic state after pragma GCC diagnostic pop with -Werror 
*/
/* { dg-message "warnings being treated as errors" "" {target "*-*-*"} 0 } */

void testing2() {
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wstrict-overflow"
  int j = 4;
  j + 4 < j;
#pragma GCC diagnostic pop
}

void testing3() {
  int k = 4;
  k + 4 < k; /* { dg-error "overflow" } */
}

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

  switch (e) /* { dg-warning "switch" } */
    {
    case ONE:
      r = 1;
      break;
    }
  return r;
}
