// GROUPS passed operators
// opr-mm file
// Date: Thu, 2 Jun 94 10:00:29 +0200
// From: chatty@cenatls.cena.dgac.fr (Stephane Chatty)
// Message-Id: <9406020800.AA14201@geant.cenatls.cena.dgac.fr>
// Subject: result of operator -- with g++-2.5.8

#include <stdio.h>

void nop()
{
}

int main ()
{
        int a = 2;

        if (----a == 0)
		nop ();

	if (a == 0)
		printf("PASS\n");
	else
		{ printf("FAIL\n"); return 1; }
}
