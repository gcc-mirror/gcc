// { dg-do assemble  }
// GROUPS passed ARM-compliance
// arm file
// Message-Id: <9303161105.AA29336@slsvitt>
// From: dcb@us-es.sel.de (David Binderman 3841)
// Subject: Page 141 of the ARM
// Date: Tue, 16 Mar 93 12:05:24 +0100

struct K {
      void f( int *p  = 0); // { dg-error "" } previous specification
};

extern int * q;

void K::f( int *p = q);// { dg-error "" } .*

