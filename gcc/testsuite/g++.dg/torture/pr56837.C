// { dg-do run }
// { dg-options "-ftree-loop-distribute-patterns" }

extern "C" void abort (void);
extern "C" int memcmp (const void *, const void *, __SIZE_TYPE__);

bool b1[8];
bool b2[8] = { true, true, true, true, true, true, true, true };

int main()
{
  unsigned int i;
  for(i=0 ; i < 8; i++)
    b1[i] = true;

  if (memcmp (b1, b2, 8) != 0)
    abort ();

  return 0;
}
