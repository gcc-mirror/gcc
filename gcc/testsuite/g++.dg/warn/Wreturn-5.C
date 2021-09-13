// PR C++/66590
// { dg-do compile }
// { dg-options "-Wall" }

struct A{ ~A();};

int f(int x)
{
    A a;
    switch (x)
    {
        case 1: { A tmp; return 1; } break;
        default: return 0;
    }
}	// { dg-bogus "control reaches end of non-void function" }
