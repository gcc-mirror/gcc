// PR tree-optimization/99305
// { dg-do compile }
// { dg-options "-O3 -fno-ipa-icf -fdump-tree-optimized" }
// { dg-final { scan-tree-dump-times " = \\\(unsigned char\\\) c_\[0-9]*\\\(D\\\);" 3 "optimized" { target { ! unsigned_char } } } }
// { dg-final { scan-tree-dump-times " = \[^\n\r]* \\+ \[0-9]*;" 3 "optimized" } }
// { dg-final { scan-tree-dump-times " = \[^\n\r]* <= 9;" 3 "optimized" } }
// { dg-final { scan-tree-dump-not "if \\\(c_\[0-9]*\\\(D\\\) \[!=]= 0\\\)" "optimized" } }
// { dg-final { scan-tree-dump-not " = PHI <" "optimized" } }

bool
foo (char c)
{
  return c >= 48 && c <= 57;
}

bool
bar (char c)
{
  return c != 0 && foo (c);
}

bool
baz (char c)
{
  return c != 0 && c >= 48 && c <= 57;
}
