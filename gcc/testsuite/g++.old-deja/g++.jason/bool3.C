// From: panisset@cae.ca (Jean-Francois Panisset)
// Subject: Problem with constant expressions for bitfields
// Date: Mon, 6 Jun 94 14:00:01 EDT

// Bug: g++ doesn't treat boolean true and false as constant values.
// Build don't link:

enum E { e1,e2,e3,e4,e5 };

struct X
{
        unsigned int bits : ((e5 > 4) ? 8 : 4);	// gets bogus error - constant expression
};
