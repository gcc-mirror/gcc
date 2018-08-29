/* { dg-lto-do link } */
/* { dg-lto-options {{-r -nostdlib -flto -g}} } */
/* { dg-extra-ld-options "-flinker-output=nolto-rel" } */

long Perl_my_htonl (long l)
{
      union { } u;
}
