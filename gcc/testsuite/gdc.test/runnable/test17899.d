module test17899;

// Test that the ICE in 13259 does not ICE but produces correct code
auto dg = delegate {}; 

int setme = 0;
void delegate() bar1 = (){ setme = 1;};

__gshared void delegate() bar2 = (){ setme = 2;};

void main()
{
    dg();
    assert(setme == 0);
    bar1();
    assert(setme == 1);
    bar2();
    assert(setme == 2);
}
