// Build don't link: 
// GROUPS passed labels
class X {
public:
    X();
};
void foo ()
{
X:  ::abort();
    goto X;
}
