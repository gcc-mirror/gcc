/* { dg-do compile } */
/* { dg-options "-O1 -fdump-tree-vars-details" } */

void link_error();

struct OOf {
        int value;
        OOf() {value = 0;}
};
inline OOf operator+(OOf op1, OOf op2)
{
        OOf f;
        f.value = op1.value + op2.value;
        return f;
}
inline OOf operator*(OOf op1, OOf op2)
{
        OOf f;
        f.value = op1.value * op2.value;
        return f;
}
inline OOf operator-(OOf op1, OOf op2)
{
        OOf f;
        f.value = op1.value - op2.value;
        return f;
}
inline OOf test_func(
        OOf a,
        OOf b,
        OOf c
)
{
        OOf d, e;
        OOf result;
        d = a * b + b * c;
        e = a * c - b * d;
        result = d * e;
        return result;
}

void test()
{
  OOf a, b, c;
  OOf d = test_func (a,b,c);
  if (d.value)
    link_error();
}

/* We should have removed the casts from pointers to references and caused SRA to happen.  */
/* { dg-final { scan-tree-dump-times "link_error" 0 "vars"} } */
/* { dg-final { cleanup-tree-dump "vars" } } */
