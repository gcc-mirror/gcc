// { dg-do assemble  }
// GROUPS passed arg-matching
// From: gustavo@cpqd.br (Gustavo Chaves)
// Date:     Wed, 25 May 94 09:38:00 EST
// Subject:  problem with user defined conversions in initialization
// Message-ID: <9405251238.AA19815@moon.cpqd.br>

struct String { String(const char*); };

struct Ack { Ack(String); };

struct S { void method(Ack); };	// { dg-message "candidate is" } referenced below

void function(Ack);

int
foo(S *o)
{ // Neither call has a usable constructor for conversions of char[5] to Ack.
  function("adsf");// { dg-error "conversion" } 
  o->method("adsf");// { dg-error "no matching" } 
  return 0;
}
