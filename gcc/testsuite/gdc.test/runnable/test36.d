// PERMUTE_ARGS:

import std.stdio;
interface IUnknown{
        extern(Windows):
        void func();
}
class ComObject :IUnknown
{
extern (Windows):
        void func()
        {writefln(`comobject`);
        }
}
interface IDataObject: IUnknown
{
        extern(Windows):
        void method();
}
package class invarianttest:ComObject, IDataObject
{
        invariant()
        {
                writefln(`hello invariant`);
        }

extern (Windows):
        override void func()
        {
        int esp;
        version(GNU)
        {
            version(X86) asm
            {
                "mov %%ESP,%0" : "=r" esp : : ;
            }
            else version(X86_64) asm
            {
                "mov %%ESP,%0" : "=r" esp : : ;
            }
            else version(ARM) asm
            {
                "str sp,%0" : "=m" esp : : ;
            }
            else static assert(false, "ASM code not implemented for this architecture");
        }
        else asm
        {
                mov esp,ESP;
        }
        printf("\n%d",esp);
        printf(`func`);
        }
        void method()
        {
                writefln(`method`);
        }
}
int main()
{
        auto inst= new invarianttest;
        int esp;
        version(GNU)
        {
            version(X86) asm
            {
                "mov %%ESP,%0" : "=r" esp : : ;
            }
            else version(X86_64) asm
            {
                "mov %%ESP,%0" : "=r" esp : : ;
            }
            else version(ARM) asm
            {
                "str sp,%0" : "=m" esp : : ;
            }
            else static assert(false, "ASM code not implemented for this architecture");
        }
        else asm
        {
                mov esp,ESP;
        }
        inst.func();
        inst.method();
        writefln("\n%d",esp);
        version(GNU)
        {
            version(X86) asm
            {
                "mov %%ESP,%0" : "=r" esp : : ;
            }
            else version(X86_64) asm
            {
                "mov %%ESP,%0" : "=r" esp : : ;
            }
            else version(ARM) asm
            {
                "str sp,%0" : "=m" esp : : ;
            }
            else static assert(false, "ASM code not implemented for this architecture");
        }
        else asm
        {
                mov esp,ESP;
        }
        writefln("\n%d",esp);
        return 0;
}

