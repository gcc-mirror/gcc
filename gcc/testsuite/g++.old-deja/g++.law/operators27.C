// { dg-do run  }
// GROUPS passed operators
// opr-new file
// From: David Binderman 3841 <dcb@us-es.sel.de>
// Date:     Mon, 21 Jun 93 11:42:11 +0200
// Subject:  G++ 2.4.3 and operator new
// Message-ID: <9306210942.AA10276@slsvitt.us-es.sel.de>

int FLAG=0;

#include <new>

extern "C" int printf( const char *, ...);

void * operator new(size_t, const std::nothrow_t&) throw()         { FLAG=1; return 0; }

class K {
private:
        int i;
public:
        K( int j) {
                i = j;
        }
};

int main(void)
{
    K * pK = new (std::nothrow) K( 10);
    if ( FLAG != 1 )
	{ printf ("FAIL\n"); return 1; }
    else
	printf ("PASS\n");
    return 0;
}
