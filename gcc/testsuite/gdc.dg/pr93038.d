// https://gcc.gnu.org/bugzilla/show_bug.cgi?id=93038
// { dg-options "-J $srcdir/gdc.dg/fileimports -MMD" }
// { dg-do compile }
// { dg-final { scan-file pr93038.deps "(^|\\n)pr93038.o:" } }
// { dg-final { scan-file pr93038.deps "pr93038.d" } }
// { dg-final { scan-file pr93038.deps "pr93038.txt" } }
// { dg-final { file delete pr93038.deps } }
module pr93038;

const VERSION = import("pr93038.txt");
