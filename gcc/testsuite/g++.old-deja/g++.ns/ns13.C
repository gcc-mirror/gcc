namespace std{ 
  void f(){}
  void g();
  int i=5;
}

void std::g()
{}

int main()
{
  return std::i-5;
}
