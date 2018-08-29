/* { dg-lto-do link } */
/* { dg-require-effective-target fpic } */
/* { dg-lto-options {{-fPIC -r -nostdlib -flto}} } */
/* { dg-extra-ld-options "-flinker-output=nolto-rel" } */

/* Empty file.  See PR41173.  */
