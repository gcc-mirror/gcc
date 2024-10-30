/* C2Y N3370 - Case range expressions.  */
/* { dg-do compile } */
/* { dg-options "-std=gnu23 -pedantic -W -Wall" } */

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
    case -2 ... -1:			/* { dg-warning "ISO C does not support range expressions in switch statements before C2Y" } */
					/* { dg-warning "conversion of '-2' to 'unsigned int' in range expression changes value to '\[0-9]*'" "" { target *-*-* } .-1 } */
					/* { dg-warning "conversion of '-1' to 'unsigned int' in range expression changes value to '\[0-9]*'" "" { target *-*-* } .-2 } */
      break;
    case ~0U + 1ULL ... ~0U + 2ULL:	/* { dg-warning "ISO C does not support range expressions in switch statements before C2Y" } */
					/* { dg-warning "conversion of '\[0-9]*' to 'unsigned int' in range expression changes value to '0'" "" { target *-*-* } .-1 } */
					/* { dg-warning "conversion of '\[0-9]*' to 'unsigned int' in range expression changes value to '1'" "" { target *-*-* } .-2 } */
      break;
    case 70 ... 70:			/* { dg-warning "ISO C does not support range expressions in switch statements before C2Y" } */
      break;
    case 80 ... 78:			/* { dg-warning "ISO C does not support range expressions in switch statements before C2Y" } */
					/* { dg-warning "empty range specified" "" { target *-*-* } .-1 } */
      break;
    case -4 ... -4:			/* { dg-warning "ISO C does not support range expressions in switch statements before C2Y" } */
					/* { dg-warning "conversion of '-4' to 'unsigned int' in range expression changes value to '\[0-9]*'" "" { target *-*-* } .-1 } */
      break;
    case ~0U + 6ULL ... ~0U + 6ULL:	/* { dg-warning "ISO C does not support range expressions in switch statements before C2Y" } */
					/* { dg-warning "conversion of '\[0-9]*' to 'unsigned int' in range expression changes value to '5'" "" { target *-*-* } .-1 } */
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
    case -32 ... -30:			/* { dg-warning "ISO C does not support range expressions in switch statements before C2Y" } */
					/* { dg-warning "case label value is less than minimum value for type" "" { target *-*-* } .-1 } */
      break;
    case -31:				/* { dg-error "duplicate case value" } */
      break;
    case -42:				/* { dg-warning "case label value is less than minimum value for type" } */
      break;
    case -43 ... -41:			/* { dg-warning "ISO C does not support range expressions in switch statements before C2Y" } */
					/* { dg-error "duplicate \\\(or overlapping\\\) case value" "" { target *-*-* } .-1 } */
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
    case -6 ... -8:			/* { dg-warning "ISO C does not support range expressions in switch statements before C2Y" } */
					/* { dg-warning "empty range specified" "" { target *-*-* } .-1 } */
      break;
    case -6:
      break;
    case -7:
      break;
    case -8:
      break;
    case F...G:				/* { dg-warning "ISO C does not support range expressions in switch statements before C2Y" } */
      break;
    case -10:
      break;
    case -10 ... -11:			/* { dg-warning "ISO C does not support range expressions in switch statements before C2Y" } */
					/* { dg-warning "empty range specified" "" { target *-*-* } .-1 } */
      break;
    case -14 ... -15:			/* { dg-warning "ISO C does not support range expressions in switch statements before C2Y" } */
					/* { dg-warning "empty range specified" "" { target *-*-* } .-1 } */
      break;
    case -14 ... -15:			/* { dg-warning "ISO C does not support range expressions in switch statements before C2Y" } */
					/* { dg-warning "empty range specified" "" { target *-*-* } .-1 } */
      break;
    default:
      break;
    }
}
