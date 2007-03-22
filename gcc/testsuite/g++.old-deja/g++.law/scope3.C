// { dg-do assemble  }
// GROUPS passed scoping
// local-class file
// From: dcb@us-es.sel.de
// Date:     Fri, 27 Nov 92 15:34:28 +0100
// Subject:  GNU G++ 2.3.1 bug report
// Message-ID: <9211271434.AA15612@us-es.sel.de>


void f()
{
        {
                struct A {
                        A() {}
                } a;
        };
        {
                struct A {
                        A() {}
                } a ;
        };
}
