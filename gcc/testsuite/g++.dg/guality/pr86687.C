// PR debug/86687
// { dg-do run }
// { dg-options "-g" }

class string {
public:
  string (int p) { this->p = p ; }
  string (const string &s) { this->p = s.p; }

  int p;
};

class foo {
public:
  foo (string dir_hint) {
    p = dir_hint.p; // { dg-final { gdb-test . "dir_hint.p" 3 } }
  }

  int p;
};

int
main (void)
{
  string s = 3;
  foo bar(s);
  return !(bar.p == 3);
}
