// REQUIRED_ARGS: -w
// https://issues.dlang.org/show_bug.cgi?id=13858

void foo() { assert(0); }

void main()
{
    int x = 0;

    LSwitch: switch (x)
    {
        case 0:
            break LSwitch;

        default: return;
    }

    foo();
}

