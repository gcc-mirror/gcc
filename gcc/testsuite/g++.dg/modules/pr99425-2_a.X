// PR 99425 template aliases can cause equivalences in the hash
// tables, and for extra excitement be reordered on rehash.

// { dg-additional-options {-x c++-system-header stdexcept -fmodules-ts} }
// { dg-prune-output {linker input file unused} }
// { dg-skip-if "requires hosted libstdc++ for stdexcept" { ! hostedlib } }

No! DO NOT COMPILE;
