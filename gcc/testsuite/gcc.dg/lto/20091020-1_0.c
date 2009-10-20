/* { dg-lto-do link } */
/* { dg-lto-options {{-fPIC -shared -flto}} } */

typedef struct {
    int NumPackStreams;
} CSzAr;
void cli_7unz (CSzAr db) { }

