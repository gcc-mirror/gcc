/* { dg-lto-do link } */
/* { dg-lto-options {{-shared}} } */
int exported_var = 42;
/* { dg-final { scan-symbol "exported_var" } } */
