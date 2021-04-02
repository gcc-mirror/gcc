// { dg-do compile }
// PR 98943, compiler feature tests can get confused by not linking
// { dg-options "NOTAFILE" }

int main ()
{
  return 0;
}

// { dg-regexp {[^\n:]*: warning: NOTAFILE: linker input file unused because linking not done\n[^\n:]*: error: NOTAFILE: linker input file not found: [^\n]*\n} }
