/* { dg-do compile } */
/* Check for volatile behaviour.  */
extern int xxx (void);
volatile extern int xxx (void);  /* { dg-error "not compatible" } */
