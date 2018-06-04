/* { dg-lto-do link } */
/* { dg-require-effective-target fpic } */
/* { dg-lto-options {{-fPIC -r -nostdlib -flto}} } */
/* { dg-extra-ld-options "-flinker-output=nolto-rel" } */

typedef struct {
    int NumPackStreams;
} CSzAr;
void cli_7unz (CSzAr db) { }

