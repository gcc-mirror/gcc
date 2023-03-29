/* Check that we get the right warning for nonint main in freestanding. */
/* { dg-do compile } */
/* { dg-options "-ffreestanding -Wreturn-type" } */

const char *main() {} /* { dg-warning "-Wreturn-type" } */
