/* PR c++/103012 Exception handling with multiversioned functions */
/* { dg-do run } */
/* { dg-require-ifunc "" }  */

extern "C" void abort (void);

__attribute__((target("default")))
void f() {
    throw 1;
}

__attribute__((target("sse4.2,bmi")))
void f() {
    throw 2;
}

int main()
{
    try {
        f();
    }
    catch(...)
    {
        return 0;
    }

    abort ();
}
