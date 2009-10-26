/* { dg-lto-do link } */
/* { dg-lto-options {{-fPIC -r -nostdlib -flto}} } */

typedef struct {
    int NumPackStreams;
} CSzAr;
void cli_7unz (CSzAr db) { }

