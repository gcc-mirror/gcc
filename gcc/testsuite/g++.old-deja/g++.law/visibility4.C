// { dg-do assemble  }
// GROUPS passed visibility
// visibility file
// From: dcb@us-es.sel.de (David Binderman 3841)
// Date:     Tue, 30 Mar 93 15:48:47 +0200
// Subject:  page 242 of the ARM
// Message-ID: <9303301348.AA20751@slsvitt>

class A {
public:
     int b; // { dg-error "" } private
};

class C : private A {                   // NOTE WELL. private, not public
public:
        int d;
};

extern "C" int printf( const char *, ...);

class E : public C {
        void f() {
                printf( "%d\n", b);// { dg-error "" } .*
        };
};
