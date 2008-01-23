// Test for PR c++/34829
// Placement new should be ok for non-aggregate Java types.

// { dg-do compile }
// { dg-options "" }

extern "Java"
{
  typedef __java_byte jbyte;
}

typedef __SIZE_TYPE__ size_t;

void *operator new (size_t, void *m)
{
  return m;
}

jbyte *f(void *memory)
{
  return new (memory) jbyte;
}
