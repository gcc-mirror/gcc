/* PR preprocessor/71183 */
/* { dg-do preprocess } */
/* { dg-set-compiler-env-var SOURCE_DATE_EPOCH "630333296" } */

const char *date = __DATE__;
const char *time = __TIME__;

/* { dg-final { scan-file source_date_epoch-3.i "Dec 22 1989" } } */
/* { dg-final { scan-file source_date_epoch-3.i "12:34:56" } } */
