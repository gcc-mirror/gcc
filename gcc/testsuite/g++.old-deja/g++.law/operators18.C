// Build don't link: 
// GROUPS passed operators
// opr-ampr file
// From: mecklen@oops.cs.utah.edu (Robert Mecklenburg)
// Date:     Thu, 8 Oct 92 16:33:08 -0600
// Subject:  Type conversion and overloading bugs in 2.2.2
// Message-ID: <9210082233.AA28203@hellgate.utah.edu>

        enum E { a, b, c };
        struct Estr {
            E value;
            Estr()                              {}
            Estr( int i ) : value( (E)i )       {}
            operator E()                        { return value; }
        };
        extern Estr baz();
        int bazz() { return baz() & 2; }
