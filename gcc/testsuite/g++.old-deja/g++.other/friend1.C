// Build don't link:
// f() should be able to access B::j, as of FDIS [class.protected]/1

// Subject: Re: [bug] Inheritance and friend access control broken
// References: <199803032141.WAA09332@piano.dptmaths.ens-cachan.fr>
// <orhg5ff544.fsf@iguacu.dcc.unicamp.br>
// <199803041125.MAA06937@cor.dptmaths.ens-cachan.fr>
// <orn2f6ek92.fsf@iguacu.dcc.unicamp.br> <19980304102900.46897@dgii.com>
// From: Alexandre Oliva <oliva@dcc.unicamp.br>
// Date: 06 Mar 1998 01:43:18 -0300


class B {
protected:
    int i;
    static int j;
};

class D : public B {
    friend void f();
};

void f()
{
    ((B*)0)->i = 3; // ERROR - protected
    ((D*)0)->i = 4;
    B::j = 5;			// gets bogus error - XFAIL *-*-*
    D::j = 6;
}
