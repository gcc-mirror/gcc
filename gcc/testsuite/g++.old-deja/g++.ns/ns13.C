namespace std{ 
  void f(){}
  void g();
  int i=5;
}

void std::g()
{}

main()
{
  return std::i-5;
}
