/* { dg-do compile } */
/* { dg-options "-O2 -fno-strict-aliasing -fwrapv -fexcess-precision=standard" } */
extern int foo();
typedef struct {
  char id;
  unsigned char fork_flags;
  short data_length;
} Header;
int a;
void X() {
  do {
    char* b;
    Header c;
    if (a)
      c.fork_flags |= 1;
    __builtin_memcpy(b, &c, __builtin_offsetof(Header, data_length));
    b += foo();
  } while (1);
}
