// Build don't link:

void f() 
{
  union {
  private:
    int i; // ERROR - private
  } u;

  u.i = 3; // ERROR - within this context
}
