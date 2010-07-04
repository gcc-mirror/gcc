/* { dg-lto-do link } */
/* { dg-lto-options {{-r -nostdlib -flto -g}} } */

long Perl_my_htonl (long l)
{
      union { } u;
}
