/*
PERMUTE_ARGS:
EXTRA_SOURCES: imports/mod2.d
RUN_OUTPUT:
---
EvilOne
---
*/

// mod1.d

import imports.mod2;

string name()
{
  return "EvilOne";
}

int main(string[] args)
{
  greet();
  return 0;
}
