// { dg-do assemble  }
// GROUPS passed operators
// opr-new file
// From: rwave!myersn%rwave.roguewave@cs.orst.edu (Nathan Myers)
// Date:     Wed, 16 Dec 92 11:55 PST
// Subject:  2.3.2: friend decl breaks member op new
// Message-ID: <m0n24qP-0000GmC@rwave.roguewave.com>

#include <stddef.h>
struct Link {
  void* operator new(size_t, int);
  friend void* __builtin_new(size_t);  // This declaration triggers the bug
};
void f() { new(2) Link; }
