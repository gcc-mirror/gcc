void __open_alias(int, ...) __asm__("open");
void __open_alias(int flags, ...) {}
extern __inline __attribute__((__gnu_inline__)) int open() {}
struct {
  void *func;
} a = {open};

int main()
{
  return 0;
}
