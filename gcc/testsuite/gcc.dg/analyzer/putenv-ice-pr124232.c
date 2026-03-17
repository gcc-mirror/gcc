extern int putenv (char *__string)
  __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__ (1)));

struct {
  char s[16];
} e = { "a=b" };

int main(int, char *[]) {
  return putenv(e.s);
}
