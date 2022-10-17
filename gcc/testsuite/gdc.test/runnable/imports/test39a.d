module imports.test39a;

extern (C) int printf (const char*, ...);

class Test (T)
{
        final void show (in T[] msg)
        {
                printf ("%.*s\n", cast(int)msg.length, msg.ptr);
        }
}

Test!(char) Global;
