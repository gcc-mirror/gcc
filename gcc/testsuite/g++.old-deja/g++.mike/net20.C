// Build don't link:
// A pointer to member function test case.

struct X
{
};

void (X::* fee ())()
{
 lab: goto lab;
 return 0;
}
