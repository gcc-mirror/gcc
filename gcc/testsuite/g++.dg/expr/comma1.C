// { dg-do run }

extern "C" void abort ();

struct gtst
{
  unsigned char data[2];
};

static struct gtst s;

int main(int argc, char *argv[])
{
  unsigned char * pc;
  struct gtst * ps;
  ps = &s;
  pc = (ps->data[0]='A', ps->data);
  if (&s.data[0] != pc)
    abort();
  return 0;
}
