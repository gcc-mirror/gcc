// { dg-do assemble  }
// GROUPS passed static-mem
// static-mem file
// Message-Id: <9406021639.AA00789@oz.NeXT.COM>
// From: Sumana Srinivasan <Sumana_Srinivasan@next.com>
// Date: Thu,  2 Jun 94 09:39:09 -0700
// Subject: static members function pointers

class CRTFooBar;

class CRTFoo {
public:
        static const CRTFooBar & defaultFooBar( );

        CRTFoo( const CRTFoo & );
        CRTFoo( );
        CRTFoo( const char *,
                const CRTFooBar &tp = CRTFoo::defaultFooBar(),
                int = 0 );
        CRTFoo &setFoo( double,
                const CRTFooBar & = CRTFoo::defaultFooBar() );

};
