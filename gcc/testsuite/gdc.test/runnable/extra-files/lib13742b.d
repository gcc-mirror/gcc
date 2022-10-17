module lib13742b;
import lib13742a;

void clear()
{
    void foo() {} // nested function
    performLocked!foo; // template from other module (preceding on command line)
}
