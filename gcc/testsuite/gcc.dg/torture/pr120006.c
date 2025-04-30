/* { dg-do run } */
/* { dg-additional-options "-fipa-pta" } */

char *b;
int f = 1;

char *xstrdup(char *i) {
  char *c = __builtin_strdup(i);
  if (!c)
    __builtin_exit(1);
  return c;
}

int main() {
  char g;
  char h[8];

  for (int i = 0; i < 2; i++) {
    char c = *__builtin_strdup("");
    b = &g;

    if (f) {
      h[0] = '-';
      h[1] = 'a';
      h[2] = '\0';
      b = xstrdup(h);
   }
  }
  if (__builtin_strcmp(b, "-a") != 0)
    __builtin_abort();
}
