/* { dg-do compile } */
// PR116510

char excmap_def_0;
int gg_strescape_i;
void gg_strescape() {
  for (; gg_strescape_i; gg_strescape_i++)
    switch ((unsigned char)gg_strescape_i)
    case '\\':
    case '"':
      excmap_def_0 = 0;
}
