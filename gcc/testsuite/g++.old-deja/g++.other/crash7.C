// Build don't link:

void f() 
{
  union {
  private:
    int i;
  } u;

  u.i = 3; // ERROR - private
}
