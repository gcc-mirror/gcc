// Build don't link:
// Origin: Mark Mitchell <mark@codesourcery.com>

struct S 
{
};

struct T : public S 
{
};

struct U : public T 
{
};

void f (U);

int main ()
{
  U u;
  f (u);
}
