// PR tree-optimization/71625 - missing strlen optimization on different
// array initialization style
//
// Verify that strlen() calls with constant character array arguments
// initialized with string constants are folded.  (This is a small
// subset of pr71625).
// { dg-do compile }
// { dg-options "-O0 -Wno-error=narrowing -fdump-tree-gimple" }

#define A(expr) do { typedef char A[-1 + 2 * !!(expr)]; } while (0)

/* This is undefined but accepted without -Wpedantic.  Verify that
   the size is zero.  */
const char ax[] = { };

void size0 ()
{
  A (sizeof ax == 0);
}

const char a0[] = { 'a', 'b', 'c', '\0' };

int len0 ()
{
  return __builtin_strlen (a0);
}

// Verify that narrowing warnings are preserved.
const signed char
sa0[] = { 'a', 'b', 255, '\0' };    // { dg-warning "\\\[\(-Wnarrowing|-Woverflow\)" "" { target { ! c++98_only } } }

int lens0 ()
{
  return __builtin_strlen ((const char*)sa0);
}

const unsigned char
ua0[] = { 'a', 'b', -1, '\0' };     // { dg-warning "\\\[\(-Wnarrowing|-Woverflow\)" "" { target { ! c++98_only } } }

int lenu0 ()
{
  return __builtin_strlen ((const char*)ua0);
}

const char c = 0;
const char a1[] = { 'a', 'b', 'c', c };

int len1 ()
{
  return __builtin_strlen (a1);
}

template <class T>
int tmplen ()
{
  static const T
    a[] = { 1, 2, 333, 0 };         // { dg-warning "\\\[\(-Wnarrowing|-Woverflow\)" }
  return __builtin_strlen (a);
}

template int tmplen<char>();

const wchar_t ws4[] = { 1, 2, 3, 4 };
const wchar_t ws7[] = { 1, 2, 3, 4, 0, 0, 0 };
const wchar_t ws9[9] = { 1, 2, 3, 4, 0 };

void wsize ()
{
  A (sizeof ws4 == 4 * sizeof *ws4);
  A (ws4[0] == 1 && ws4[1] == 2 && ws4[2] == 3 && ws4[3] == 4);

  A (sizeof ws7 == 7 * sizeof *ws7);
  A (ws7[0] == 1 && ws7[1] == 2 && ws7[2] == 3 && ws7[4] == 4
     && !ws7[5] && !ws7[6]);

  A (sizeof ws9 == 9 * sizeof *ws9);
  A (ws9[0] == 1 && ws9[1] == 2 && ws9[2] == 3 && ws9[4] == 4
     && !ws9[5] && !ws9[6] && !ws9[7] && !ws9[8]);
}

#if 0

// The following aren't handled.

const char &cref = c;
const char a2[] = { 'a', 'b', 'c', cref };

int len2 ()
{
  return __builtin_strlen (a2);
}


const char* const cptr = &cref;
const char a3[] = { 'a', 'b', 'c', *cptr };

int len3 ()
{
  return __builtin_strlen (a3);
}

#endif

// { dg-final { scan-tree-dump-times "strlen" 0 "gimple" } }
