// PR 30891
// { dg-do compile }

int main() {
  int i = 0;
  namespace foo { // { dg-error "'namespace' definition is not allowed here" } 
    int j = 0;
  }
  return 0;
}
