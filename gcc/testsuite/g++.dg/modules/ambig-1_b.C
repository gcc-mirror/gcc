// { dg-additional-options -fmodules-ts }
import One;

int foo (); // { dg-error "conflicts with import" }
int bax (); // { dg-error "ambiguating new declaration" }
int quux (int);
