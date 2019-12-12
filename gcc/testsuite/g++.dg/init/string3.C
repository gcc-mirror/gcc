// PR tree-optimization/71625 - missing strlen optimization on different
// array initialization style
//
// Verify that strlen() call with a constant character array argument
// initialized with non-constant elements isn't folded.
//
// { dg-do compile }
// { dg-options "-O2 -fdump-tree-optimized" }


extern const char c;
const char a0[] = { 'a', 'b', 'c', c };

int len0 ()
{
  return __builtin_strlen (a0);
}

const char &ref = c;
const char a1[] = { 'a', 'b', 'c', ref };

int len1 ()
{
  return __builtin_strlen (a1);
}

const char* const ptr = &c;
const char a2[] = { 'a', 'b', 'c', *ptr };

int len2 ()
{
  return __builtin_strlen (a2);
}

// { dg-final { scan-tree-dump-times "strlen" 3 "optimized" } }
