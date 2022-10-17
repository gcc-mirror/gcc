// https://gcc.gnu.org/bugzilla/show_bug.cgi?id=101640
// { dg-do compile }
// { dg-options "-fdump-tree-original" }

int fun101640(ref int);

int test101640(int val)
{
    // { dg-final { scan-tree-dump "= val \\\+ fun101640 \\\(\\\(int &\\\) &val\\\);" "original" } }
    return val + fun101640(val);
}
