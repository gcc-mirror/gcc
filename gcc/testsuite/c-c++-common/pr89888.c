/* PR c/89888 */
/* { dg-do compile { target { int32 } } } */
/* { dg-options "" } */

long long y;

void
foo (unsigned char x)
{
  switch (x)
    {
    case -1: y = -1; break;			/* { dg-message "previously used here" } */
						/* { dg-warning "case label value is less than minimum value for type" "" { target *-*-* } .-1 } */
    case 0xffffffff: y = 0xffffffff; break;	/* { dg-error "duplicate case value" } */
    case ~0U: y = ~0U; break;			/* { dg-error "duplicate case value" } */
    }
}

void
bar (unsigned char x)
{
  switch (x)
    {
    case -1: y = -1; break;			/* { dg-message "previously used here" } */
						/* { dg-warning "case label value is less than minimum value for type" "" { target *-*-* } .-1  } */
    case -1: y = -1; break;			/* { dg-error "duplicate case value" } */
    case -1: y = -1; break;			/* { dg-error "duplicate case value" } */
    }
}

void
baz (unsigned char x)
{
  switch (x)
    {
    case -7: y = -7; break;			/* { dg-warning "case label value is less than minimum value for type" } */
    case -5 ... 2: y = -5; break;		/* { dg-warning "lower value in case label range less than minimum value for type" } */
    case 18: y = 18; break;
    case (unsigned char) -2 ... 4 + (unsigned char) -2: y = 2; break;	/* { dg-warning "upper value in case label range exceeds maximum value for type" } */
    case 24 + (unsigned char) -2: y = 3; break;	/* { dg-warning "case label value exceeds maximum value for type" } */
    }
}

void
qux (unsigned char x)
{
  switch (x)
    {
    case (unsigned char) -1 ... 1 + (unsigned char) -1: y = 2; break;	/* { dg-warning "upper value in case label range exceeds maximum value for type" } */
    case -12: y = -7; break;			/* { dg-warning "case label value is less than minimum value for type" } */
    case 18: y = 18; break;
    case 27 + (unsigned char) -1: y = 3; break;	/* { dg-warning "case label value exceeds maximum value for type" } */
    case -1 ... 0: y = -5; break;		/* { dg-warning "lower value in case label range less than minimum value for type" } */
    }
}

void
quux (unsigned char x)
{
  switch (x)
    {
    case (unsigned char) -2 ... (unsigned char) -1: y = 2; break;
    case 18: y = 18; break;
    case 1 + (unsigned char) -1: y = 3; break;	/* { dg-warning "case label value exceeds maximum value for type" } */
    case -2 ... -1: y = -5; break;		/* { dg-warning "case label value is less than minimum value for type" } */
    }
}
