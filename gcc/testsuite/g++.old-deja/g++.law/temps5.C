// GROUPS passed temps
// temps file
// Message-Id: <9311171029.AA00592@mencon>
// From: gfm@mencon.mencon.oz.au (Graham Menhennitt)
// Subject: gcc 2.5.3 - bug deleting object that is still referred to
// Date: Wed, 17 Nov 93 21:29:23 EST

#include        <stdio.h>

class C {
public:
        C(int i) : val(i) { ; }
        C(const C& c) : val(c.val) { ; }
        ~C(void) { val = 999; }
        C& operator = (const C& c) { val = c.val; return *this; }

        C& inc(int i) { val += i; return *this; }

        int val;
};

C
f(void)
{
        return C(3);
}

C
f(int i)
{
        return f().inc(i);
}

int
main(void)
{
	if (f (2).val != 5)
		{ printf ("FAIL\n"); return 1; }
	else
		printf ("PASS\n");
}
