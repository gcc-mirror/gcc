/* Test diagnostics for switch statements and labels therein.  Test
   with no special options.  */
/* Origin: Joseph Myers <joseph@codesourcery.com> */
/* { dg-do compile } */
/* { dg-options "" } */

void
f (int a, double d, void *p)
{
  switch (d) /* { dg-error "error: switch quantity not an integer" } */
    {
    }
  switch (p) /* { dg-error "error: switch quantity not an integer" } */
    {
    }
  switch (a)
    {
    case (void *)0: ; /* { dg-error "error: pointers are not permitted as case values" } */
    }
  switch (a)
    {
    case (double)0: ; /* { dg-error "error: case label does not reduce to an integer constant" } */
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
    case 0 ... -1: ; /* { dg-warning "warning: empty range specified" } */
    }
  switch (a)
    {
    case 0 ... -2: ; /* { dg-warning "warning: empty range specified" } */
    }
  switch (a)
    {
    case 0:
    default: /* { dg-error "error: this is the first default label" } */
    case 1:
    default: ; /* { dg-error "error: multiple default labels in one switch" } */
    }
  switch (a)
    {
    case 0: /* { dg-error "error: previously used here" } */
    case 1:
    case 0: ; /* { dg-error "error: duplicate case value" } */
    }
 case 1: ; /* { dg-error "error: case label not within a switch statement" } */
 default: ; /* { dg-error "error: 'default' label not within a switch statement" } */
   break; /* { dg-error "error: break statement not within loop or switch" } */
   continue; /* { dg-error "error: continue statement not within a loop" } */
   switch (a)
     {
     case a: ; /* { dg-error "error: case label does not reduce to an integer constant" } */
     }
   switch (a)
     {
     case 0: /* { dg-error "error: this is the first entry overlapping that value" } */
     case -1 ... 1: /* { dg-error "error: duplicate \\(or overlapping\\) case value" } */
     case 2 ... 3: /* { dg-error "error: previously used here" } */
     case 2: /* { dg-error "error: duplicate case value" } */
     case 4 ... 7: /* { dg-error "error: this is the first entry overlapping that value" } */
     case 6 ... 9: ; /* { dg-error "error: duplicate \\(or overlapping\\) case value" } */
     }
   switch (a)
     {
     case 0:
       continue; /* { dg-error "error: continue statement not within a loop" } */
     }
}
