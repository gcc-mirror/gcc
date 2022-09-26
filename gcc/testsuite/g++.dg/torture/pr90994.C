// { dg-do compile }
// { dg-additional-options "-fnon-call-exceptions -Wuninitialized" }

extern void printval(unsigned char v);

inline int readbyte(unsigned char *__restrict presult,
		    unsigned char volatile *ptr)
{
  unsigned char v;
  try {
      v = *ptr;
  } catch (...) {
      return -1;
  }
  *presult = v;
  return 0;
}

int incorrectWarning(unsigned char volatile *ptr)
{
  int error;
  unsigned char first;
  unsigned char second;

  error = readbyte(&first, ptr);
  asm("\n\n\n\n\n" : : "X" (error != 0));
  if (error != 0)
    goto err;

  error = readbyte(&second, ptr);
  if (error != 0)
    goto err;

  printval(first);   // { dg-bogus "uninitialized" }
  printval(second);
  return 0;

err:
  return error;
}
