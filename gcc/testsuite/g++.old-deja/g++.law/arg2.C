// Build don't link: 
// GROUPS passed arg-matching
// arg-matching file
// Message-Id: <199303032114.AA03574@kolvir.Boulder.ParcPlace.COM>
// From: Warner Losh <imp@boulder.parcplace.com>
// Subject: Overloading bug in g++ 2.3.3 (sparc) compiled by GNU C version 2.3.3
// Date: Wed, 03 Mar 1993 14:14:02 MST

class c1 { };

typedef void    (*fnp)(void *);
typedef void    (c1::*memfnp)(void *);
extern  void    fn1( fnp );
extern  void    fn1( c1*, memfnp );

void f3(void *) { }

void fn2()
{
        fn1((fnp) &f3);         // Everybody likes this
        fn1(&f3);               // g++ complains here  line 13
}
