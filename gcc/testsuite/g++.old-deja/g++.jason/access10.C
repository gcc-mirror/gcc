// PRMS Id: 4839
// Bug: The initializer of a static member of a class has the same acess
// rights as a member function.  g++ doesn't realize that.
// Build don't link:

class X 
{
  X (int);
  static X foo;
public:
  void dummy();
};

X X::foo = 9;
