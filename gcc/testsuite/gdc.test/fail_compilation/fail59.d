/*
TEST_OUTPUT:
---
fail_compilation/fail59.d(50): Error: outer class `C1` `this` needed to `new` nested class `C2`
---
*/

class C1
{
    int c1;

    this()
    {
        c1 = 2;
    }

    class C2
    {
        class C3
        {
            int c3;

            this(int n)
            {
                c3 = n + c1 + c2;
            }
        }

        int c2;

        C3 foo()
        {
            return new C3(8);
        }

        this(int k)
        {
            c2 = k + 7;
        }
    }

    C2 bar()
    {
        return new C2(17);
    }
}

void main()
{
    C1.C2 q = new C1.C2(3);
}
