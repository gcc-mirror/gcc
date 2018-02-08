/* PR target/83100 */
/* { dg-do compile } */
/* { dg-options "-O2 -fcommon -fdata-sections" } */
/* { dg-skip-if "-fdata-sections not supported" { hppa*-*-hpux* nvptx-*-* } } */

const int a = 7;
