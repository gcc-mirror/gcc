/* { dg-do compile } */
/* { dg-options "-Wno-implicit-fallthrough" } */

void bar (int);

void
foo (int i)
{
  switch (i)
    {
    case 1:			/* { dg-bogus "this statement may \[fla\]* through" "" { target *-*-* } .+1 } */
      bar (1);
    case 2:			/* { dg-bogus "this statement may \[fla\]* through" "" { target *-*-* } .+1 } */
      bar (2);
      /* Some comment.  */
    case 3:			/* { dg-bogus "this statement may \[fla\]* through" "" { target *-*-* } .+1 } */
      bar (3);
      /* Here we really do want to fALl   tHRoUgh and we mean it!  */
    case 4:			/* { dg-bogus "this statement may \[fla\]* through" "" { target *-*-* } .+1 } */
      bar (4);
      /* Intentionally fall through.  */
    case 5:			/* { dg-bogus "this statement may \[fla\]* through" "" { target *-*-* } .+1 } */
      bar (5);
      /* FALLTHROUGH */
    case 6:			/* { dg-bogus "this statement may \[fla\]* through" "" { target *-*-* } .+1 } */
      bar (6);
      __attribute__((fallthrough));
    case 7:			/* { dg-bogus "this statement may \[fla\]* through" "" { target *-*-* } .+1 } */
      bar (7);
    default:
      break;
    }
}
