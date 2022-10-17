/*
REQUIRED_ARGS:
EXECUTE_ARGS: 10000
RUN_OUTPUT:
---
count = 10000
70000
---
*/

extern(C) int printf(const char *, ...);
extern(C) int atoi(const char *);

    int main (string[] argv)
    {
        string s = "";
        int count, loop;

        count = atoi((argv[1] ~ '\0').ptr);
        if (count == 0)
            count = 1;
        printf("count = %u\n", count);

        for (loop = 0; loop < count; loop ++)
            s ~= "hello\n";
        for (loop = 0; loop < count; loop ++)
            s ~= "h";
        printf ("%llu\n", cast(ulong) s.length);
        //printf("%.*s\n", s[0..100]);
        assert(s.length == count * (6 + 1));
        s.length = 3;
        s.length = 10;
        s.length = 0;
        s.length = 1000;
        return 0;
    }
