/* { dg-additional-options "-O1" } */

struct Lisp_Overlay
{
  struct Lisp_Overlay *next;
};

void
test_1 (struct Lisp_Overlay *tail, long prev)
{
  long end;
  if (!tail || end < prev || !tail->next) /* { dg-warning "use of uninitialized value 'end'" } */
    return;
}

void
test_2 (struct Lisp_Overlay *tail, long prev)
{
  long end;
  if (tail && end < prev && !tail->next) /* { dg-warning "use of uninitialized value 'end'" } */
    return;
}
