// GROUPS passed builtins
// Apparently not in g++ bug snapshot (was originally sent to bug-gcc)
// Message-Id: <m0p74Fh-0002fCC@neal.ctd.comsat.com>
// Date: Tue, 7 Dec 93 10:23 EST
// From: neal@ctd.comsat.com (Neal Becker)
// Subject: builtin_alloca on hpux (gcc-2.5.6)
// We have to avoid using -ansi, which results in a call to alloca instead of
//  the use of __builtin_alloca, and thus ends up being unresolved.
// Special g++ Options: 

extern "C" void* alloca( __SIZE_TYPE__ );
extern "C" int printf (const char *, ...);

void* junk() {
  return alloca(10);
}
main() { printf ("PASS\n");}
