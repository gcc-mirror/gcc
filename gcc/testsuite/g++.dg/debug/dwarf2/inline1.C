// This isn't really testing dwarf output, but rather that we can inline f
// even though the call precedes the definition.

// { dg-options "-gdwarf-2 -dA -O" }
// { dg-final { scan-assembler "DW_TAG_inlined_subroutine" } }

template <class T>
inline T f(T);

int main()
{
  f(1);
}

int i;

template <class T>
inline T f(T t) { ++i; return t; }
