// PR c++/84471
// { dg-do compile { target c++11 } }
// { dg-additional-options -fdump-tree-gimple-lineno }
// { dg-final { scan-tree-dump-not {value-expr: \[} "gimple" } }

int main(int argc, char**)
{
  int x = 1;
  auto f = [&x, &argc](const char* i) {
    i += x;
    i -= argc;
    i += argc - x;
    return i;
  };
  f("          ");
}
