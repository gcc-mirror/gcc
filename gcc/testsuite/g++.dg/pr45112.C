/* { dg-do compile } */

struct JSString
{
  unsigned char mLength;
  static JSString unitStringTable[];
};

JSString JSString::unitStringTable[] __attribute__ ((aligned (8))) = { 1 };

int bug [__alignof__ (JSString::unitStringTable) >= 8 ? 1 : -1];

