/* { dg-do compile } */
/* { dg-options "-mcpu=fidoa" } */

/* Check that an error is issued for using more than one
   interrupt_attribute at the same time.  */

/* If the current mutilib is, say, -mcpu=5485, the compiler gets
   -mcpu=fidoa -mcpu=5485, where -mcpu=fidoa is overridden.  In that
   case, we just use two interrupt_handler attributes and expect the
   same error.  */
#ifdef __mfido___
#define IH interrupt_thread
#else
#define IH interrupt_handler
#endif

extern void f1 (void) __attribute__((interrupt_handler, interrupt_handler)); /* { dg-error "multiple interrupt attributes not allowed" } */

extern void f2 (void) __attribute__((interrupt_handler, IH)); /* { dg-error "multiple interrupt attributes not allowed" } */

extern void f3 (void) __attribute__((IH, interrupt_handler)); /* { dg-error "multiple interrupt attributes not allowed" } */

extern void f4 (void) __attribute__((IH, IH)); /* { dg-error "multiple interrupt attributes not allowed" } */
