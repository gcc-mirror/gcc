/* PR target/55717  */

/* { dg-do compile } */
/* { dg-options "-O2 -march=z10 -fPIC" } */

extern char temp[];
short ansi_value[256];
void terminal_state(void)
{
  static const char *puc[] = { "", "<", "=", ">", "?", 0};
  int i, j, k, l, modes_found;
  char buf[256];
  k = (int) __builtin_strlen(temp);
  for (j = l = 0; j < 255 && j - l < 50; j++)
    {
      __builtin_sprintf(temp, "\033[%s%d$p", puc[i], j);
      if (ansi_value[1])
        {
          l = j;
          buf[k] = '\0';
          put_crlf();
          ptextln(buf);
          buf[k++] = ' ';
          k = (int) __builtin_strlen(temp);
        }
    }
  for (i = j = 0; j < modes_found; j = ++i >> 1)
    ;
}
