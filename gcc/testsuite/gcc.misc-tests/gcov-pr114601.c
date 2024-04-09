/* PR gcov-profile/114601 */
/* { dg-do compile } */
/* { dg-options "-fcondition-coverage -finstrument-functions-once" } */

/* -finstrument-functions-once inserts a hidden conditional expression into
   this function which otherwise has none.  This caused a crash on looking up
   the condition as the cond->expr map is not created unless it necessary.  */
void
empty (void)
{
}
