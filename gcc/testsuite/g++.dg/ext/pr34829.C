// Test for PR c++/34829
// Placement new should be ok for non-aggregate Java types.

// { dg-do compile }
// { dg-options "" }

extern "Java"
{
  typedef __java_byte jbyte;
}

void *operator new (unsigned int s, void *m)
{
  return m;
}

jbyte *f(void *memory)
{
  return new (memory) jbyte;
}
