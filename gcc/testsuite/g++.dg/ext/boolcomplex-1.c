/* { dg-do compile } */
/* { dg-options "" } */
bool b = --0i == 0; /* { dg-error "lvalue required as decrement operand" } */
