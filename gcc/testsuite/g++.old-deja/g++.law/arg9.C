// { dg-do assemble  }
// GROUPS passed arg-matching
// arg-matching file
// Message-Id: <199405132049.QAA06835@elan.cs.UMD.EDU>
// Subject: Bug in g++ 2.4.5 and 2.5.8
// Date: Fri, 13 May 1994 16:49:22 -0400
// From: Evan Rosser <ejr@cs.umd.edu>

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>

class TupleIterator {
public:
    TupleIterator(int *tpl);
    int& operator*();
    int  live() const;
// The compile fails with "no post-increment operator for type" at "TI++"
// below.
// It succeeds with the same declarations if set_position does not take an int.
// This occurs with G++ 2.4.5 and 2.5.8.
// Sun CC works OK with either case.
        void operator++(int);
        void set_position(int);
private:
};

int main() {

int t[5];
t[1] = 1; t[2] = 2;t[3] = 3;t[4] = 4;
TupleIterator TI(t);

    while(TI.live()){
        printf("%d", *TI);
        TI++;
    }
}

