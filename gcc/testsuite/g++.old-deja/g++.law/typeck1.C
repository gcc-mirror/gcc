// Build don't link: 
// GROUPS passed typeck
// typeck file
// From: vern@daffy.ee.lbl.gov (Vern Paxson)
// Date:     24 Sep 1992 23:11:22 GMT
// Subject:  2.2.2 type-checking error (?) when comparing pointers
// Message-ID: <26475@dog.ee.lbl.gov>


        class a { };
        class foo : a { };
        class bar : a { };

        int test( const foo* f, const bar* b )
                {
                return f == b;// ERROR - 
                }

