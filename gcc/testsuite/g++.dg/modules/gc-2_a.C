// { dg-additional-options "-fmodules-ts --param ggc-min-expand=0 --param ggc-min-heapsize=0 -fno-module-lazy -fmodule-mapper=[srcdir]/gc-2.map" }
// { dg-additional-files map-1.map }

// Make sure the module hash table survives GC

// { dg-module-cmi "=map-1_a.nms" }
export module frob;

int thing;
