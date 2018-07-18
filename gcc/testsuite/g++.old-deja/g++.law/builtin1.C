// { dg-do run  }
// { dg-options "" }
// { dg-require-effective-target alloca }
// GROUPS passed builtins
// Apparently not in g++ bug snapshot (was originally sent to bug-gcc)
// Message-Id: <m0p74Fh-0002fCC@neal.ctd.comsat.com>
// Date: Tue, 7 Dec 93 10:23 EST
// From: neal@ctd.comsat.com (Neal Becker)
// Subject: builtin_alloca on hpux (gcc-2.5.6)
// We have to avoid using -ansi, which results in a call to alloca instead of
//  the use of __builtin_alloca, and thus ends up being unresolved.

extern "C" int printf (const char *, ...);

void* junk() {
  return __builtin_alloca(10);
}

int
main() { printf ("PASS\n"); return 0; }
