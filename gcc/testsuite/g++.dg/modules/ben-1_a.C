// { dg-additional-options "-fmodules-ts -fmodule-mapper=[srcdir]/ben-1.map" }
// { dg-additional-files ben-1.map }

export module module:import;
// { dg-module-cmi =partitions/module:import.mod }

export int b() {
  return 0;
}
