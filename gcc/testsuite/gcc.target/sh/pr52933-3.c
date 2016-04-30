/* Check that the div0s instruction is used for integer sign comparisons.  */
/* { dg-do compile }  */
/* { dg-options "-O2" } */
/* { dg-final { scan-assembler-times "div0s" 2 } } */

typedef struct { unsigned int arg[100]; } *FunctionCallInfo;
typedef struct { int day; int month; } Interval;
void* palloc (unsigned int);
int bar (void);
void baz (void);

void
interval_pl (FunctionCallInfo fcinfo)
{
  Interval *span1 = ((Interval *) ((char *) ((fcinfo->arg[0]))));
  Interval *span2 = ((Interval *) ((char *) ((fcinfo->arg[1]))));
  Interval *result = (Interval *) palloc (sizeof (Interval));

  /* Should emit 2x div0s.  */
  if ((((span1->month) < 0) == ((span2->month) < 0))
      && !(((result->month) < 0) == ((span1->month) < 0)))
    do {
      if (bar ())
	baz ();
    } while(0);
  result->day = span1->day + span2->day;
}
