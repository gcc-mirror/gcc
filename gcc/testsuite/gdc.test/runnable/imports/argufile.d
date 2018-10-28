// argufile.d ----------------------------------------------------

public:

import core.vararg;
import std.stdio;
import std.utf;

dstring formatstring(TypeInfo[] arguments, va_list argptr)
{

	dstring message = null;

	void putc(dchar c)
	{
		message ~= c;
	}


	doFormat(&putc, arguments, argptr);


	return message;
}

string arguments(...) // turns a bunch of arguments into a formatted char[] string
{
	return std.utf.toUTF8(formatstring(_arguments, _argptr));
}

void useargs(...)
{
	string crashage = arguments("why is 8 scared of 7? because", 7,8,9);

	//printf("%.*s\n", crashage);
	writefln(crashage);
}


// dustmited version of the deprecated doFormat.
// See the full file at:
// https://github.com/dlang/undeaD/blob/master/src/undead/doformat.d
void doFormat(void delegate(dchar) putc, TypeInfo[] arguments, va_list ap)
{
    import core.stdc.stdlib : alloca, malloc;
    import std.format ;

    size_t bufLength = 1024;
    void* argBuffer = malloc(bufLength);
    size_t bufUsed ;
    foreach (ti; arguments)
    {
        auto pos = bufUsed;
        // Align to next word boundary
        bufUsed += ti.tsize + size_t.sizeof - 1;
        bufUsed -= bufUsed& size_t.sizeof - 1;
        // Copy argument into buffer
        va_arg(ap, ti, argBuffer + pos);
    }

    auto argptr = argBuffer;
    void* skipArg(TypeInfo ti)
    {
        auto p = argptr;
        // Align to next word boundary
        argptr += ti.tsize + size_t.sizeof - 1;
        argptr -= cast(size_t)argptr & size_t.sizeof - 1;
        return p;
    }
    auto getArg(T)()
    {
        return *cast(T*)skipArg(typeid(T));
    }

    TypeInfo ti;
    Mangle m;
    void formatArg()
    {
        ulong vnumber;
        char vchar;
        Mangle m2;
        int signed ;
        string s;

        void putstr(const char[] s)
        {
            foreach (c; s)
                putc(c);

        }

        //printf("formatArg(fc = '%c', m = '%c')\n", fc, m);
        int mi;
        switch (m)
        {
            L2:
                putstr((&vchar)[0 .. 1]);
                return;

            case Mangle.Tint:
                signed = 1;
                vnumber = getArg!int;
                goto Lnumber;

            case Mangle.Tarray:
                mi = 10;
while (1)
                {
                    m2 = cast(Mangle)typeid(ti).name[mi];
                    switch (m2)
                    {
                        case Mangle.Tchar:
                            s = getArg!string;
                            putstr(s);
                            break;

                        case Mangle.Timmutable:
                            mi++;
                            continue;

                        default:
                            {}
                    }
                    return;
                }
            default:
                {}
        }

    Lnumber:
;
vchar = cast(char)('0' + vnumber);
                goto L2;
    }

    for (int j ; j < arguments.length; )
    {
        ti = arguments[j++];
        int mi = 9;
        do
            m = cast(Mangle)typeid(ti).name[mi++];
while (m == Mangle.Tconst );

            formatArg;
    }
}
