// Bug: The double cast had an TREE_INT_CST_HIGH of 0, while the single
// cast had -1, so the comparison failed.

// { dg-do run }

struct A { };

typedef int A::* aip;
typedef long A::* alp;

int main()
{
  return ((aip)(alp)0 != (aip)0);
}
