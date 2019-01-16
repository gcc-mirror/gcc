module imports.a19a;

struct Dummy
{
}

struct TemplatedStruct(Param)
{
        static real fgh = 0;
}

void foo()
{
        alias TemplatedStruct!(Dummy) X;        
}
