// Build don't link: 
// GROUPS passed parsing
// parsing folder
// From: hendrik%vedge.UUCP@iro.umontreal.ca
// Date:     Wed, 23 Sep 92 17:10:28 -0400
// Subject:  parenthesized method
// Message-ID: <9209232110.AA02533@.>


class goo{
public:
      void noo_bloo();
};

void choo(goo* too)
{     (too->noo_bloo)();
}

void choo_too(goo* too)
{     too->noo_bloo();
}
