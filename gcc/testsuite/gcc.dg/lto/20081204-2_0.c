/* { dg-lto-do link } */
/* { dg-skip-if "" { ! { i?86-*-* x86_64-*-* } } } */
/* { dg-lto-options {{-w -flto -fPIC -r -nostdlib}} } */
/* { dg-extra-ld-options "-flinker-output=nolto-rel" } */

register int ri asm("edi");
