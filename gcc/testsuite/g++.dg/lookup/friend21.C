// Unhiding a friend erroneously mutated a binding

class X 
{
  friend void frob (int, int);
};
  
void frob (int);
void frob (int, int);

void foo ()
{
  frob (1); // Only saw unhidden friend
}
