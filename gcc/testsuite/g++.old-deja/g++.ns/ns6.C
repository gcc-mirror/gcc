namespace A{
 int i;
 namespace B{
  void f(){i++;}
  int i;
  void g(){i++;}
  }
}

main()
{
  return A::i-A::B::i;
}
