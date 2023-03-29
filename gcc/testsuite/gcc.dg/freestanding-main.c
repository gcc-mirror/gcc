/* Make sure we don't get a missing return warning on freestanding. */
/* { dg-do compile } */
/* { dg-options "-ffreestanding -Wreturn-type" } */

int main() {}
