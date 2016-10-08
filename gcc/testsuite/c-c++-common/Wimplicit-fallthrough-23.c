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
    case 5:
      bar (5);			/* { dg-bogus "this statement may \[laf]* through" } */
      /* Else Fall-Thru!  */
    case 6:
      bar (6);
      break;
    case 7:
      bar (7);			/* { dg-bogus "this statement may \[laf]* through" } */
      /* Some comment.  */
      /* ... fallthrough ...  */
      /* Some other comment.  */
      /* And yet another.  */
    case 8:
      bar (8);
      break;
    case 9:
      bar (9);			/* { dg-bogus "this statement may \[laf]* through" } */
      /* Intentional Fallthru */
    case 10:
      bar (10);
      break;
    case 11:
      bar (11);			/* { dg-bogus "this statement may \[laf]* through" } */
      /* intentionally fall through  */
    case 12:
      bar (12);
      break;
    case 13:
      bar (13);			/* { dg-bogus "this statement may \[laf]* through" } */
      /* Falls Through - for reasons known only to the author.  */
    case 14:
      bar (14);
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
    case 19:
      bar (19);			/* { dg-bogus "this statement may \[laf]* through" } */
      /*lint -fallthrough*/
    case 20:
      bar (20);
      break;
    case 21:
      bar (21);			/* { dg-bogus "this statement may \[laf]* through" } */
      /*lint -fallthrough */
    case 22:
      bar (22);
      break;
    case 23:
      bar (23);			/* { dg-bogus "this statement may \[laf]* through" } */
      /*fallthru*/
    case 24:
      bar (24);
      break;
    case 25:
      bar (25);			/* { dg-bogus "this statement may \[laf]* through" } */
      /*Else fallthru*/
    case 26:
      bar (26);
      break;
    case 27:
      bar (27);			/* { dg-bogus "this statement may \[laf]* through" } */
      /*Intentional fallthru*/
    case 28:
      bar (28);
      break;
    case 29:
      bar (29);			/* { dg-bogus "this statement may \[laf]* through" } */
      /*Intentionally fallthru*/
    case 30:
      bar (30);
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
    case 35:
      bar (35);			/* { dg-bogus "this statement may \[laf]* through" } */
      /* Else, fall-through. */
    case 36:
      bar (36);
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
    case 5:
      bar (5);			/* { dg-bogus "this statement may \[laf]* through" } */
      // Else Fall-Thru!
    case 6:
      bar (6);
      break;
    case 7:
      bar (7);			/* { dg-bogus "this statement may \[laf]* through" } */
      // Some comment.
      // ... fallthrough ...
      // Some other comment.
      // And yet another.
    case 8:
      bar (8);
      break;
    case 9:
      bar (9);			/* { dg-bogus "this statement may \[laf]* through" } */
      // Intentional Fallthru
    case 10:
      bar (10);
      break;
    case 11:
      bar (11);			/* { dg-bogus "this statement may \[laf]* through" } */
      // intentionally fall through 
    case 12:
      bar (12);
      break;
    case 13:
      bar (13);			/* { dg-bogus "this statement may \[laf]* through" } */
      // Falls Through - for reasons known only to the author.
    case 14:
      bar (14);
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
    case 19:
      bar (19);			/* { dg-bogus "this statement may \[laf]* through" } */
      //lint -fallthrough
    case 20:
      bar (20);
      break;
    case 21:
      bar (21);			/* { dg-bogus "this statement may \[laf]* through" } */
      //lint -fallthrough 
    case 22:
      bar (22);
      break;
    case 23:
      bar (23);			/* { dg-bogus "this statement may \[laf]* through" } */
      //fallthru
    case 24:
      bar (24);
      break;
    case 25:
      bar (25);			/* { dg-bogus "this statement may \[laf]* through" } */
      //Else fallthru
    case 26:
      bar (26);
      break;
    case 27:
      bar (27);			/* { dg-bogus "this statement may \[laf]* through" } */
      //Intentional fallthru
    case 28:
      bar (28);
      break;
    case 29:
      bar (29);			/* { dg-bogus "this statement may \[laf]* through" } */
      //Intentionally fallthru
    case 30:
      bar (30);
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
    case 35:
      bar (35);			/* { dg-bogus "this statement may \[laf]* through" } */
      // Else, fall-through
    case 36:
      bar (36);
      break;
    default:
      break;
    }
}
