// Build don't link: 
// GROUPS passed labels

extern "C" void abort();

class X {
public:
    X();
};
void foo ()
{
X:  ::abort();
    goto X;
}
