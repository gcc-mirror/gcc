/* { dg-lto-do link } */
/* { dg-lto-options { { -flto -g } } } */
/* { dg-extra-ld-options {-r -nostdlib -flinker-output=nolto-rel} } */

void gfc_be_parse_file (void)
{
  typedef enum builtin_type builtin_type;
}
