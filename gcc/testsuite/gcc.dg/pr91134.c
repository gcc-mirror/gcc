/* PR c/91134 */
/* { dg-options "-fdiagnostics-show-caret" } */

struct X { int member; } x;
struct Y { struct X **x; } y;

int
foo (void)
{
  struct X *pointer = &x;
  struct Y *yp = &y;
  struct X **pointerpointer = &pointer;
  int i = *pointerpointer->member;	/* { dg-error "'pointerpointer' is a pointer to pointer; did you mean to dereference it before applying '->' to it\\\?" } */
/* { dg-begin-multiline-output "" }
   int i = *pointerpointer->member;
                          ^~
            (*            )
   { dg-end-multiline-output "" } */
  int j = pointer.member;		/* { dg-error "'pointer' is a pointer; did you mean to use '->'\\\?" } */
/* { dg-begin-multiline-output "" }
   int j = pointer.member;
                  ^
                  ->
   { dg-end-multiline-output "" } */
  int k = yp->x->member;		/* { dg-error "'yp->x' is a pointer to pointer; did you mean to dereference it before applying '->' to it\\\?" } */
/* { dg-begin-multiline-output "" }
   int k = yp->x->member;
                ^~
           (*   )
   { dg-end-multiline-output "" } */
  return i + j + k;
}
