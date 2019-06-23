// PR c++/68265

int main()
{
  int (*) {} // { dg-error "expected primary-expression" }
         any amount of syntactic nonsense // { dg-error "not declared in this scope" }
         on multiple lines, with *punctuation* and ++operators++ even...
         will be silently discarded
         until the next close brace
}
