/* { dg-do run } */

#define MAX_LENGTH 96
#define SEQUENCE_LENGTH 31

static struct {
  char buf[MAX_LENGTH + 1];
} u1, u2;

extern void abort (void);

int main ()
{
  int i;
  char c;

  for (i = 0, c = 'A'; i < MAX_LENGTH; i++, c++)
    {
      u1.buf[i] = 'a';
      if (c >= 'A' + SEQUENCE_LENGTH)
	c = 'A';
      u2.buf[i] = c;
    }
  if (u1.buf[MAX_LENGTH] != '\0')
    abort ();

  return 0;
}
