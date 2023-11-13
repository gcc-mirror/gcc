int
main(void)
{
  static char static_char_array[1];
  static char *static_char_pointer;
  static char static_char;
  char  char_array[1];
  char *char_pointer;
  char  character;

  char *cp, c;

  c = cp - static_char_array;   /* error */
  c = cp - static_char_pointer;
  c = cp - &static_char;        /* error */
  c = cp - char_array;
  c = cp - char_pointer;
  c = cp - &character;
}
