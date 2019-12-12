// PERMUTE_ARGS: -version=X -inline -release -g -O
// EXTRA_SOURCES: imports/link2644a.d
// EXTRA_SOURCES: imports/link2644b.d
// EXTRA_SOURCES: imports/link2644c.d
// COMPILE_SEPARATELY:

module link2644;
import imports.link2644a;

void main()
{
  version(X)
    auto c = new X.CA();
  else
    auto c = new CA();
}
