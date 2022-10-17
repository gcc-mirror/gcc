//https://issues.dlang.org/show_bug.cgi?id=19315
void main()
{
    #line 100 "file.d"
    enum code = q{
        #line 10
    };
    static assert(__LINE__ == 103);
    static assert(__FILE__ == "file.d");

    #line 200 "file2.d"
    enum code2 = q{
        q{
            #line 5
        }
        #line 9 "foo.d"
    };
    static assert(__LINE__ == 206);
    static assert(__FILE__ == "file2.d");
}
