template <class STRUCT, class MEMBER> inline STRUCT *
setback(MEMBER *bp, MEMBER STRUCT::*offset)
{
        if(!bp) return 0;
        union { int i; MEMBER STRUCT::*of; } u;
        u.of = offset;
        return (STRUCT *) ((int) bp - u.i);
}
 

struct S
{
  int i;
};

int main()
{
  S s;

  S* sp = setback (&s.i, &S::i);
}
