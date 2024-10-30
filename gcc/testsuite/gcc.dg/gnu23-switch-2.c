/* C2Y N3370 - Case range expressions.  */
/* { dg-do compile } */
/* { dg-options "-std=gnu23 -pedantic -W -Wall -Wno-c23-c2y-compat" } */

enum E { F = 10, G = 20 };

void
foo (unsigned x)
{
  switch (x)
    {
    case -1:
      break;
    case ~0U + 1ULL:			/* { dg-warning "conversion from 'long long unsigned int' to 'unsigned int' changes value from '\[0-9]*' to '0'" } */
      break;
    default:
      break;
    }
}

void
bar (unsigned x)
{
  switch (x)
    {
    case -2 ... -1:			/* { dg-warning "conversion of '-2' to 'unsigned int' in range expression changes value to '\[0-9]*'" } */
					/* { dg-warning "conversion of '-1' to 'unsigned int' in range expression changes value to '\[0-9]*'" "" { target *-*-* } .-1 } */
      break;
    case ~0U + 1ULL ... ~0U + 2ULL:	/* { dg-warning "conversion of '\[0-9]*' to 'unsigned int' in range expression changes value to '0'" } */
					/* { dg-warning "conversion of '\[0-9]*' to 'unsigned int' in range expression changes value to '1'" "" { target *-*-* } .-1 } */
      break;
    case 70 ... 70:
      break;
    case 80 ... 78:			/* { dg-warning "empty range specified" } */
      break;
    case -4 ... -4:			/* { dg-warning "conversion of '-4' to 'unsigned int' in range expression changes value to '\[0-9]*'" } */
      break;
    case ~0U + 6ULL ... ~0U + 6ULL:	/* { dg-warning "conversion of '\[0-9]*' to 'unsigned int' in range expression changes value to '5'" } */
      break;
    default:
      break;
    }
}

void
baz (unsigned char x)
{
  switch (x)
    {
    case -32 ... -30:			/* { dg-warning "case label value is less than minimum value for type" } */
      break;
    case -31:				/* { dg-error "duplicate case value" } */
      break;
    case -42:				/* { dg-warning "case label value is less than minimum value for type" } */
      break;
    case -43 ... -41:			/* { dg-error "duplicate \\\(or overlapping\\\) case value" } */
      break;
    default:
      break;
    }
}

void
qux (int x)
{
  switch (x)
    {
    case -6 ... -8:			/* { dg-warning "empty range specified" } */
      break;
    case -6:
      break;
    case -7:
      break;
    case -8:
      break;
    case F...G:
      break;
    case -10:
      break;
    case -10 ... -11:			/* { dg-warning "empty range specified" } */
      break;
    case -14 ... -15:			/* { dg-warning "empty range specified" } */
      break;
    case -14 ... -15:			/* { dg-warning "empty range specified" } */
      break;
    default:
      break;
    }
}
