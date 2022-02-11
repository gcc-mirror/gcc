/* REQUIRED_ARGS: -unittest
   PERMUTE_ARGS:
 */

static foreach(s; ["666", "777", "888"])
{
    mixin(genTest(s));
}

int i;

string genTest(string a)
{
    return "unittest { i += " ~ a ~ "; }";
}

void main()
{
    assert(i == 0 + 666 + 777 + 888);
}
