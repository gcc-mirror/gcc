// Make sure the module hash table survices GC

// { dg-additional-options "--param ggc-min-expand=0 --param ggc-min-heapsize=0 -fno-module-lazy -fmodule-mapper=$srcdir/g++.dg/modules/gc-2.map" }
// { dg-additional-files map-1.map }

// { dg-module-bmi "=map-1_a.nms" }
export module frob;

int thing;
