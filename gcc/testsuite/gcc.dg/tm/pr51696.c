/* { dg-do compile } */
/* { dg-options "-fgnu-tm" } */

struct list {
  void (*compare)();
} *listPtr;

static void (*compare)();

__attribute__((transaction_safe))
static void func () {
  listPtr->compare(); /* { dg-error "unsafe indirect function call" } */
  compare(); /* { dg-error "unsafe function call" } */
}
