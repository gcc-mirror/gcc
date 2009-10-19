/* { dg-do run } */
/* { dg-require-iconv "ISO-8859-2" } */
/* { dg-options "-std=gnu99 -fexec-charset=ISO-8859-2" } */

const char *str1 = "h\u00e1\U0000010Dky ";
const char *str2 = "\u010d\u00E1rky\n";
const char *str3 = u8"h\u00e1\U0000010Dky ";
const char *str4 = u8"\u010d\u00E1rky\n";
const char *str5 = "h\u00e1\U0000010Dky " "\u010d\u00E1rky\n";
const char *str6 = u8"h\u00e1\U0000010Dky " "\u010d\u00E1rky\n";
const char *str7 = "h\u00e1\U0000010Dky " u8"\u010d\u00E1rky\n";
#define u8
const char *str8 = u8"h\u00e1\U0000010Dky " u8"\u010d\u00E1rky\n";

const char latin2_1[] = "\x68\xe1\xe8\x6b\x79\x20";
const char latin2_2[] = "\xe8\xe1\x72\x6b\x79\n";
const char utf8_1[] = "\x68\xc3\xa1\xc4\x8d\x6b\x79\x20";
const char utf8_2[] = "\xc4\x8d\xc3\xa1\x72\x6b\x79\n";

int
main (void)
{
  if (__builtin_strcmp (str1, latin2_1) != 0
      || __builtin_strcmp (str2, latin2_2) != 0
      || __builtin_strcmp (str3, utf8_1) != 0
      || __builtin_strcmp (str4, utf8_2) != 0
      || __builtin_strncmp (str5, latin2_1, sizeof (latin2_1) - 1) != 0
      || __builtin_strcmp (str5 + sizeof (latin2_1) - 1, latin2_2) != 0
      || __builtin_strncmp (str6, utf8_1, sizeof (utf8_1) - 1) != 0
      || __builtin_strcmp (str6 + sizeof (utf8_1) - 1, utf8_2) != 0
      || __builtin_strncmp (str7, utf8_1, sizeof (utf8_1) - 1) != 0
      || __builtin_strcmp (str7 + sizeof (utf8_1) - 1, utf8_2) != 0
      || __builtin_strncmp (str8, utf8_1, sizeof (utf8_1) - 1) != 0
      || __builtin_strcmp (str8 + sizeof (utf8_1) - 1, utf8_2) != 0)
    __builtin_abort ();
  if (sizeof ("a" u8"b"[0]) != 1
      || sizeof (u8"a" "b"[0]) != 1
      || sizeof (u8"a" u8"b"[0]) != 1
      || sizeof ("a" "\u010d") != 3
      || sizeof ("a" u8"\u010d") != 4
      || sizeof (u8"a" "\u010d") != 4
      || sizeof (u8"a" "\u010d") != 4)
    __builtin_abort ();
  return 0;
}
