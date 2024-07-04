void abort (void);
void exit (int);

typedef enum foo E;
enum foo { e0, e1 };

struct {
  E eval;
} s;

void
p(void)
{
  abort();
}

void
f(void)
{
  switch (s.eval)
    {
    case e0:
      p();
    }
}

int
main(void)
{
  s.eval = e1;
  f();
  exit(0);
}
