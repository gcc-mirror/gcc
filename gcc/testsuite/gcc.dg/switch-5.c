/* Test diagnostics for switch statements and labels therein.  Test
   with no special options.  */
/* Origin: Joseph Myers <joseph@codesourcery.com> */
/* { dg-do compile } */
/* { dg-options "" } */

void
f (int a, double d, void *p)
{
  switch (d) /* { dg-error "switch quantity not an integer" } */
    {
    }
  switch (p) /* { dg-error "switch quantity not an integer" } */
    {
    }
  switch (a)
    {
    case (void *)0: ; /* { dg-error "pointers are not permitted as case values" } */
    }
  switch (a)
    {
    case (double)0: ; /* { dg-error "case label does not reduce to an integer constant" } */
    }
  switch (a)
    {
    case (char)0: ;
    }
  switch (a)
    {
    case 0 ... 0: ;
    }
  switch (a)
    {
    case 0 ... -1: ; /* { dg-warning "empty range specified" } */
    }
  switch (a)
    {
    case 0 ... -2: ; /* { dg-warning "empty range specified" } */
    }
  switch (a)
    {
    case 0:
    default: /* { dg-error "this is the first default label" } */
    case 1:
    default: ; /* { dg-error "multiple default labels in one switch" } */
    }
  switch (a)
    {
    case 0: /* { dg-error "previously used here" } */
    case 1:
    case 0: ; /* { dg-error "duplicate case value" } */
    }
 case 1: ; /* { dg-error "case label not within a switch statement" } */
 default: ; /* { dg-error "'default' label not within a switch statement" } */
   break; /* { dg-error "break statement not within loop or switch" } */
   continue; /* { dg-error "continue statement not within a loop" } */
   switch (a)
     {
     case a: ; /* { dg-error "case label does not reduce to an integer constant" } */
     }
   switch (a)
     {
     case 0: /* { dg-error "this is the first entry overlapping that value" } */
     case -1 ... 1: /* { dg-error "duplicate \\(or overlapping\\) case value" } */
     case 2 ... 3: /* { dg-error "previously used here" } */
     case 2: /* { dg-error "duplicate case value" } */
     case 4 ... 7: /* { dg-error "this is the first entry overlapping that value" } */
     case 6 ... 9: ; /* { dg-error "duplicate \\(or overlapping\\) case value" } */
     }
   switch (a)
     {
     case 0:
       continue; /* { dg-error "continue statement not within a loop" } */
     }
}
