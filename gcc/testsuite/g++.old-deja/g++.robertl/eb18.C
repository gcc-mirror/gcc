// Build don't link:
class C { };

void foo()
{
    C c;
    void * v = static_cast<void *>(c);  // ERROR - illegal cast
}
