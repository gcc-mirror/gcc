// { dg-do assemble  }
// GROUPS passed pointers-to-members
class X   {
 private:
  int i;
 public:
  X(int k)			{ i=k; }
  int operator=(X &a)		{ return i = a.i; }
  int operator=(int ii)	{ return i = ii; }
};
int main(void)
{
  int (X::*op1_ptr)(X&);
  op1_ptr = &X::operator=;    // g++ gives error 
  return 0;
}
