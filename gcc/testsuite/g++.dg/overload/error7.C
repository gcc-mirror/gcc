// PR c++/115239

bool foo(char *, long);     // #1, strictly viable, ambig with #2
bool foo(char *, unsigned); // #2, strictly viable, ambig with #1
bool foo(char, long);       // #3, non-strictly viable
bool foo(char, unsigned);   // #4, non-strictly viable

int main() {
  foo((char *)0, 0); // { dg-error "ambiguous" }
}
