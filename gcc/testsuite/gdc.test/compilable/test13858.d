// REQUIRED_ARGS: -w
// 13858

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

