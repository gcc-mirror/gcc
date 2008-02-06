// PR c++/35056
// { dg-do compile }
// { dg-options "-O2" }

enum EBorderStyle { bla = 1 };
inline bool compare_ref(const unsigned int &t, const EBorderStyle &u)
{ return t == u; }
inline bool compare_val(const unsigned int t, const EBorderStyle u)
{ return t == u; }
struct S {
  unsigned m_style : 4;
};
void call_ref (S *s, EBorderStyle v)
{ if (!compare_ref(s->m_style, v)) s->m_style = v; }
void call_val (S *s, EBorderStyle v)
{ if (!compare_val(s->m_style, v)) s->m_style = v; }
