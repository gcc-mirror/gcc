// Build don't link: 
// GROUPS passed niklas dwarf
struct S { S(); };

inline void
foo (
      S a,
      S b
    )
{}

void
bar (S s1, S s2)
{ foo (s1, s2); }
