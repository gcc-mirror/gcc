void percent_x(int ch, char *p, char* ok_chars)
{
  char *cp = ch == 'a' ? p : "";
  for (;*(cp += __builtin_strspn (cp, ok_chars));)
    ;
}
