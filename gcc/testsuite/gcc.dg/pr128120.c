/* { dg-do compile } */

int v;
int b = __builtin_has_attribute (v, pure);
