// https://issues.dlang.org/show_bug.cgi?id=23406

/*
TEST_OUTPUT:
---
fail_compilation/fail23406.d(39): Error: cannot implicitly convert expression `0` of type `int` to `alphakey`
---
*/

struct flagenum
{
    int i = 1;
    alias i this;

    auto opBinary(string s)(int j)
    {
        assert(j == 1);
        return typeof(this)(i*2);
    }

    auto opEquals(int a)
    {
        return false;
    }
}

enum alphakey
{
    a = flagenum(),
    b,c,d,e,f,g,h,i,
    k,l,m,n,o,p,q,r,
    s,t,u,v,w,x,y,z
}

alphakey alpha;

void main()
{
    alpha = 0;
}
