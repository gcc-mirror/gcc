/* { dg-do compile } */
/* { dg-options "" } */
_Bool b = --0i == 0; /* { dg-error "lvalue required as decrement operand" } */
