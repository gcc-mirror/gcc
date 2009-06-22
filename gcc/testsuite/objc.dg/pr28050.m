/* { dg-do compile } */
int i = [0]; /* { dg-error "expected .:. before .\\]. token" } */
