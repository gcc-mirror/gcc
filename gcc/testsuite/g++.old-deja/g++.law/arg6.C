// { dg-do assemble  }
// GROUPS passed arg-matching
// arg-matching file
// Message-Id: <9305032310.AA03900@malachite.bbn.com>
// From: Dan Franklin <dan@diamond.bbn.com>
// Subject: overloaded function bug
// Date: Mon, 3 May 93 19:10:10 EDT


    typedef int (*fnp)(int, void*);
    void dispatch_insert(int, int, fnp, void* = 0);
    void dispatch_insert(int, int, long*, char*);
    typedef void (*InsertFunP)(int, int, fnp, void*);
    extern void f(InsertFunP);
    void g() { f(dispatch_insert); }
