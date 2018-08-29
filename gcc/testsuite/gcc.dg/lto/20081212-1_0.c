/* { dg-lto-do link } */
/* { dg-lto-options {{-r -nostdlib}} } */
/* { dg-extra-ld-options "-flinker-output=nolto-rel" } */
int exported_var = 42;
/* { dg-final { scan-symbol "exported_var" } } */
