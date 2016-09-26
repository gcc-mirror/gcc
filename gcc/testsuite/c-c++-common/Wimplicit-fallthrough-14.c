/* PR c/7652 */
/* { dg-do compile } */
/* { dg-options "-Wimplicit-fallthrough" } */

/* Test various falls through comments.  */

extern void bar (int);

void
fn (int i)
{
  switch (i)
    {
    case -1:
      bar (-1);
      /*-fallthrough*/
    case 0:
      bar (0);
      /*@fallthrough@*/
    case 1:
      bar (1);
      /* FALL THRU */
    case 2:
       bar (2);
      /* FALLTHRU */
    case 3:
      bar (3);
      /* FALLS THRU */
    case 4:
      bar (4);
      /* FALL-THRU */
    case 5:
      bar (5);
      /* FALL THROUGH */
    case 6:
       bar (6);
      /* FALLTHROUGH */
    case 7:
      bar (7);
      /* FALLS THROUGH */
    case 8:
      bar (8);
      /* FALL-THROUGH */
    case 9:
      bar (9);
      /*FALLTHRU*/
    case 10:
      bar (10);
      /* FALLTHRU.*/
    case 11:
       bar (11);
      /* FALLTHROUGH.  */
    case 12:
       bar (12);
      /* Fall thru */
    case 13:
       bar (13);
      /* Falls thru */
    case 14:
       bar (14);
      /* Fall-thru */
    case 15:
       bar (15);
      /* Fall Thru */
    case 16:
       bar (16);
      /* Falls Thru */
    case 17:
       bar (17);
      /* Fall-Thru */
    case 18:
       bar (18);
      /* Fall through */
    case 19:
       bar (19);
      /* Falls through */
    case 20:
       bar (20);
      /* Fall-through */
    case 21:
       bar (21);
      /* Fall Through */
    case 22:
       bar (22);
      /* Falls Through */
    case 23:
       bar (23);
      /* Fall-Through */
    case 24:
       bar (24);
      /* Falls through.  */
    case 25:
       bar (25);
      /*     Falls through.  */
    case 26:
       bar (26);
      /* fall thru */
    case 27:
       bar (27);
      /* falls thru */
    case 28:
       bar (28);
      /* fall-thru */
    case 29:
       bar (29);
      /* fall thru */
    case 30:
       bar (30);
      /* falls thru */
    case 31:
       bar (31);
      /* fall-thru */
    case 32:
       bar (32);
      /* fall through */
    case 33:
       bar (33);
      /* falls through */
    case 34:
       bar (34);
      /* fall-through */
    default:
      bar (99);
    }

  switch (i)
    {
    case 0:
      i++;
      /*@fallthrough@*/
L:
    default:
      bar (6);
    }

  {
    __label__ L2;
    switch (i)
      {
      case 0:
	i++;
	/*@fallthrough@*/
L2:
      default:
      bar (6);
      }
  }

  /* Don't generate false -Wswitch-unreachable warning.  */
  switch (i)
    {
      /*FALLTHROUGH*/
      case 0:
        i++;
    }

  if (i)
  {
    /* fall through */
  L1:;
  }
}
