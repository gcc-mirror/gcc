// { dg-do assemble  }
// GROUPS passed ARM-compliance
// arm file
// Message-Id: <9211191128.AA14718@us-es.sel.de>
// From: dcb@us-es.sel.de
// Subject: ARM p79 trivial bug
// Date: Thu, 19 Nov 92 12:28:24 +0100

extern "C" int printf( const char *, ...);

int main()
{
        int             a = 1 ;
        int             b = 2 ;
        int             c = 3 ;

        (a = b) = c ;
                printf( " %d %d %d\n", a, b, c);
        return 0 ;
}

