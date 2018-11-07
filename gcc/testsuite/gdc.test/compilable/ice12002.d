// REQUIRED_ARGS: -inline
// PERMUTE_ARGS:

void doFormat(void delegate(dchar) putc, TypeInfo[] arguments)
{
    void formatArg(char fc)
    {
        const(char)* prefix = "";

        void putstr(const char[] s)
        {
            //if (flags & FL0pad)
            {
                while (*prefix)
                    putc(*prefix++);
            }

            foreach (dchar c; s)
                putc(c);
        }

        putstr(null);
    }
}
