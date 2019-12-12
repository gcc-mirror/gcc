// COMPILE_SEPARATELY
// EXTRA_SOURCES: imports/link8023b.d
// PERMUTE_ARGS: -inline -release

import imports.link8023b;

private void t(alias Code)()
{
  return Code();
}

void f()
{
  t!( () { } )();
}

void main() {
  f();
}
