// PR middle-end/15069
// { dg-do run }
// { dg-options "-O2" }

extern "C" void abort (void);

typedef enum {
  FOUR = 4,
  FIVE = 5
} direction_t;

int main ()
{
  direction_t four = FOUR;
  int flags = (four & 4L) ? (32L | 128L) : 0;
  flags &= 32L;

  if (flags == 0)
    abort ();
}

