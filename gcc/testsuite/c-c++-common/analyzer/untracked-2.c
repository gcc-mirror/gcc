typedef unsigned char u8;
extern int foo(const u8 *key, unsigned int keylen);
int test (void)
{
  static const u8 default_salt[64] = {};
  return foo(default_salt, 64);
}
