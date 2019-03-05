/* { dg-do compile } */
/* { dg-skip-if "" { ! { clmcpu } } } */
/* { dg-options "-mcpu=archs -Os -w -fpic" } */

/* tst_movb split pattern is wrong for anything else than NPS
   chip.  */
__bswap_32___bsx() {
  int a = __builtin_bswap32(__bswap_32___bsx);
  if (a & 1048575)
    zlog_warn();
}
