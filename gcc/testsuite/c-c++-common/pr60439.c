/* PR c/60439 */
/* { dg-do compile } */
/* { dg-prune-output "case label value exceeds" } */

#ifndef __cplusplus
# define bool _Bool
#endif

extern bool foo (void);

void
f1 (bool b)
{
  switch (b) /* { dg-warning "switch condition has" } */
    {
    case 3:
      break;
    }
}

void
f2 (int a, int b)
{
  switch (a && b) /* { dg-warning "switch condition has" } */
    {
    case 3:
      break;
    }
  switch ((bool) (a && b)) /* { dg-warning "switch condition has" } */
    {
    case 3:
      break;
    }
  switch ((a && b) || a) /* { dg-warning "switch condition has" } */
    {
    case 3:
      break;
    }
  /* No warnings on following.  */
  switch ((int) (a && b))
    break;
  switch ((unsigned int) (a && b))
    break;
  switch ((unsigned short int) (a && b))
    break;
  switch ((char) (a && b))
    break;
}

void
f3 (int a)
{
  switch (!!a) /* { dg-warning "switch condition has" } */
    {
    case 3:
      break;
    }
  switch (!a) /* { dg-warning "switch condition has" } */
    {
    case 3:
      break;
    }
}

void
f4 (void)
{
  switch (foo ()) /* { dg-warning "switch condition has" } */
    {
    case 3:
      break;
    }
}

void
f5 (int a)
{
  switch (a == 3) /* { dg-warning "switch condition has" } */
    {
    case 3:
      break;
    }
  switch (a != 3) /* { dg-warning "switch condition has" } */
    {
    case 3:
      break;
    }
  switch (a > 3) /* { dg-warning "switch condition has" } */
    {
    case 3:
      break;
    }
  switch (a < 3) /* { dg-warning "switch condition has" } */
    {
    case 3:
      break;
    }
  switch (a <= 3) /* { dg-warning "switch condition has" } */
    {
    case 3:
      break;
    }
  switch (a >= 3) /* { dg-warning "switch condition has" } */
    {
    case 3:
      break;
    }
  switch (foo (), foo (), a >= 42) /* { dg-warning "switch condition has" } */
    {
    case 3:
      break;
    }
  switch (a == 3, a & 4, a ^ 5, a)
    break;
  switch ((int) (a == 3))
    break;
  switch ((int) (a != 3))
    break;
}

void
f6 (bool b)
{
  switch (b) /* { dg-warning "switch condition has" } */
    {
    case 3:
      break;
    }
  switch (!b) /* { dg-warning "switch condition has" } */
    {
    case 3:
      break;
    }
  switch (b++) /* { dg-warning "switch condition has" } */
  /* { dg-warning "is deprecated" "" { target c++ } 134 } */
    {
    case 3:
      break;
    }
}

void
f7 (void)
{
  bool b;
  switch (b = 1) /* { dg-warning "switch condition has" } */
    {
    case 3:
      break;
    }
}

void
f8 (int i)
{
  switch (i)
    break;
  switch ((int) i)
    break;
  switch ((unsigned int) i)
    break;
  switch ((bool) i) /* { dg-warning "switch condition has" } */
    {
    case 11:
      break;
    }
}
