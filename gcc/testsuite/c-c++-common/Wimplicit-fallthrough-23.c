/* { dg-do compile } */
/* { dg-options "-Wimplicit-fallthrough" } */

void bar (int);

void
foo (int i)
{
  switch (i)
    {
    case 1:
      bar (1);			/* { dg-bogus "this statement may \[laf]* through" } */
      /* FALLTHROUGH */
    case 2:
      bar (2);
      break;
    case 3:
      bar (3);			/* { dg-bogus "this statement may \[laf]* through" } */
      /* FALLS THRU.  */
      /* Some other comment.  */
    case 4:
      bar (4);
      break;
    case 7:
      bar (7);			/* { dg-bogus "this statement may \[laf]* through" } */
      /* Some comment.  */
      /* fallthrough.  */
      /* Some other comment.  */
      /* And yet another.  */
    case 8:
      bar (8);
      break;
    case 15:
      bar (15);			/* { dg-bogus "this statement may \[laf]* through" } */
      /*-fallthrough*/
    case 16:
      bar (16);
      break;
    case 17:
      bar (17);			/* { dg-bogus "this statement may \[laf]* through" } */
      /*@fallthrough@*/
    case 18:
      bar (18);
      break;
    case 23:
      bar (23);			/* { dg-bogus "this statement may \[laf]* through" } */
      /*fallthru*/
    case 24:
      bar (24);
      break;
    case 31:
      bar (31);			/* { dg-bogus "this statement may \[laf]* through" } */
      /*Falls thru*/
    case 32:
      bar (32);
      break;
    case 33:
      bar (33);			/* { dg-bogus "this statement may \[laf]* through" } */
      /*Fall-through*/
    case 34:
      bar (34);
      break;
    default:
      break;
    }
  switch (i)
    {
    case 1:
      bar (1);			/* { dg-bogus "this statement may \[laf]* through" } */
      // FALLTHROUGH
    case 2:
      bar (2);
      break;
    case 3:
      bar (3);			/* { dg-bogus "this statement may \[laf]* through" } */
      // FALLS THRU.  
      // Some other comment.
    case 4:
      bar (4);
      break;
    case 7:
      bar (7);			/* { dg-bogus "this statement may \[laf]* through" } */
      // Some comment.
      // fallthrough
      // Some other comment.
      // And yet another.
    case 8:
      bar (8);
      break;
    case 15:
      bar (15);			/* { dg-bogus "this statement may \[laf]* through" } */
      //-fallthrough
    case 16:
      bar (16);
      break;
    case 17:
      bar (17);			/* { dg-bogus "this statement may \[laf]* through" } */
      //@fallthrough@
    case 18:
      bar (18);
      break;
    case 23:
      bar (23);			/* { dg-bogus "this statement may \[laf]* through" } */
      //fallthru
    case 24:
      bar (24);
      break;
    case 31:
      bar (31);			/* { dg-bogus "this statement may \[laf]* through" } */
      //Falls thru
    case 32:
      bar (32);
      break;
    case 33:
      bar (33);			/* { dg-bogus "this statement may \[laf]* through" } */
      //Fall-through
    case 34:
      bar (34);
      break;
    default:
      break;
    }
}
