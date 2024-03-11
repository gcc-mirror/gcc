module issue22682;

void main()
{
    pragma(mangle, "put" ~ "s")
    extern(C) static int libcPuts(const char*);
    libcPuts("issue 22682");
}
