/* PR tree-optimization/126876 */
/* { dg-do compile } */
/* { dg-options "-O2 -w" } */

unsigned char *S_parser_feed_eol;
char S_parser_feed_end;
void S_parser_feed(unsigned char *buffer) {
  while (buffer) {
    int chunk_len;
    for (; S_parser_feed_eol;)
      chunk_len = S_parser_feed_eol - buffer;
    buffer += chunk_len;
    if (S_parser_feed_end)
      if (*buffer)
        buffer++;
  }
}

/* Second reducer, from Emacs coding.c (comment #9).  */
int b;

int c(char *p) {
  if (b) {
    p[0] = 0;
    return 1;
  }
  return 0;
}

void d(char *e) {
  while (e) {
    e += c(e);
    e += c(e);
  }
}
