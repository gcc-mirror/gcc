// { dg-do assemble  }
// { dg-options "-O" }
// GROUPS passed mentor
struct Word {
   struct S1 *p1;
};
struct S1 {
   Word o;
   void shift_left(int delta, int ct);
};
inline void S1::shift_left(int delta, int ct)
{
   int cnt = ct;
   for (S1 *to_p = this, *from_p = to_p + delta; cnt--;) *to_p++ = *from_p++;
}

void dispose_t(S1 *tp, int from_index, int ct, const int d_last_t)
{
        int new_ct = d_last_t + 1 - ct;
        tp[0].o.p1[from_index].shift_left(ct, new_ct - from_index);
}
