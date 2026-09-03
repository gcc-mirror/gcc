// Keep the source load of an inline memcpy nontrapping, but not its
// destination store.

// { dg-do compile }
// { dg-options "-O2 -fdelete-null-pointer-checks -fnon-call-exceptions -finline-stringops=memcpy -fdump-rtl-expand" }
// { dg-require-effective-target ptr32plus }
// { dg-skip-if "" keeps_null_pointer_checks }

typedef unsigned char block[16384];

void
copy (block &dest, const block &src)
{
  __builtin_memcpy (dest, src, sizeof (block));
}

// { dg-final { scan-rtl-dump {\(mem(?:/[a-z])*/c(?:/[a-z])*:} "expand" } }
// { dg-final { scan-rtl-dump {\(set[ \t\n]+\(mem(?:/[a-z])*:} "expand" } }
// { dg-final { scan-rtl-dump-not {\(set[ \t\n]+\(mem(?:/[a-z])*/c(?:/[a-z])*:} "expand" } }
// { dg-final { scan-assembler-not {\mmemcpy\M} } }
