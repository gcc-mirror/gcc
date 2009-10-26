/* { dg-lto-do link } */
/* { dg-skip-if "" { ! { i?86-*-* x86_64-*-* } } { "*" } { "" } } */
/* { dg-lto-options {{-w -flto -fPIC -r -nostdlib}} } */

register int ri asm("edi");
