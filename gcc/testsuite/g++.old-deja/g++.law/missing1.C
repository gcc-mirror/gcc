// { dg-do assemble  }
// GROUPS passed missing
// missing file
// Message-Id: <9207100259.AA11702@quaestor>
// From: adam@inference.com (David Adam)
// Subject: G++ does not allow parens around declarators.
// Date: Thu, 9 Jul 92 19:59:51 PDT

void foo()
{
  long (bar)[5];
}
