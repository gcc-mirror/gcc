/* { dg-do run } */

#define N 23
#define MAX_LEN 13
char dst[N + 1];

void __attribute__((noipa))
invert(const char *id)
{
  char buf[MAX_LEN];
  char *ptr = buf + sizeof(buf);  // start from the end of buf
  *(--ptr) = '\0';                // terminate string
  while (*id && ptr > buf) {
    *(--ptr) = *(id++);           // copy id backwards
  }
  __builtin_strncpy(dst, ptr, N);           // copy ptr/buf to dst
}


int main()
{
  invert("abcde");
  if (__builtin_strcmp(dst, "edcba"))
    __builtin_abort();
  return 0;
}
