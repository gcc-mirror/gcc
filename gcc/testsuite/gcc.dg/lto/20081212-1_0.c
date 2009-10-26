/* { dg-lto-do link } */
/* { dg-lto-options {{-r -nostdlib}} } */
int exported_var = 42;
/* { dg-final { scan-symbol "exported_var" } } */
