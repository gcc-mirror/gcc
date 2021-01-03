// { dg-additional-options "-fmodules-ts -Wno-pedantic" }
module;
# 3 __FILE__ 1

int v1;

# 9 "" 2
export module b;
// { dg-module-cmi b }

export inline auto get ()
{
  return v1;
}

