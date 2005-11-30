/* PR 24930 */
extern int foo(int u) __attribute__((noinline));
int foo(int u) {return 0;}
int
main(int argc, char** argv)
{
  const char *buf = argv[1];
  
  unsigned int data = (((unsigned int) buf[0]) << 8) + (unsigned int) buf[1];
  if (data & 0x8000) {
    data &= 0x7fff ;
    data ^= 0x7fff ;
    data += 1 ;
    data *= -1 ;
  }
  return foo((int)data);
}
