// { dg-do compile { target int128 } }
// { dg-options "" }

struct S {
  S(void) { m_i128 = 0; m_u128 = 0; }
  ~S(void) { }
  __int128 get1 (void) { return m_i128; }
  unsigned __int128 get2 (void) { return m_u128; }
  void set1 (__int128 i) { m_i128 = i; }
  void set2 (unsigned int i) { m_u128 = 1; }
  __int128 m_i128;
  unsigned __int128 m_u128;
};

struct S glb;

__int128 fo1 (void) { return glb.get1 (); }
unsigned __int128 fo2 (void) { return glb.get2 (); }
__int128 fo3 (__int128 i) { __int128 v = fo1 (); glb.set1 (i); return v; }
unsigned __int128 fo4 (unsigned __int128 i)
{
  unsigned __int128 v = fo2 (); glb.set2 (i);
  return v;
}

__int128 fo5 (__int128 i)
{
  return fo3 (i);
}

__int128 fo5 (unsigned __int128 i)
{
  return (__int128) fo4 (i);
}


// { dg-final { scan-assembler "\n_?_Z3fo1v\[: \t\n\]" } }
// { dg-final { scan-assembler "\n_?_Z3fo2v\[: \t\n\]" } }
// { dg-final { scan-assembler "\n_?_Z3fo3n\[: \t\n\]" } }
// { dg-final { scan-assembler "\n_?_Z3fo4o\[: \t\n\]" } }
// { dg-final { scan-assembler "\n_?_Z3fo5n\[: \t\n\]" } }
// { dg-final { scan-assembler "\n_?_Z3fo5o\[: \t\n\]" } }

