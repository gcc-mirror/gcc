// { dg-do run  }
// GROUPS passed ARM-compliance
// arm file
// Message-Id: <9212072127.AA24243@us-es.sel.de>
// From: dcb@us-es.sel.de
// Subject: page 78 of the ARM
// Date: Mon, 7 Dec 92 22:27:09 +0100

extern "C" int printf( const char *, ...);

int & max( int & a, int & b) {
        return (a < b) ? b : a;
}

int main( void) {
        int a = 1;
        int b = 2;
        int & c = max( a, b);

        if (&c == &b)
                printf( "PASS\n");
        else
                { printf( "FAIL\n"); return 1; }
        return 0;
}
