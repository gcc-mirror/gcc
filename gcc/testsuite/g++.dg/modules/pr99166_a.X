// PR c++/99166, diagnostic
// { dg-additional-options {-x c++-system-header iostream -fmodules-ts -flang-info-module-cmi} }

// { dg-regexp {[^\n]*iostream: note: writing CMI 'gcm.cache/[^[\n]*iostream.gcm'\n} }
// { dg-prune-output {linker input file unused} }

NO DO NOT COMPILE
