// { dg-do run  }
// GROUPS passed initialization
// init file
// From: Richard Speed <speed@cs.montana.edu>
// Date:     Sun, 6 Jun 1993 15:19:41 -0600 (MDT)
// Subject:  excess elements in aggr initzer
// Message-ID: <Pine.3.07.9306061541.A10267-b100000@fubar.cs.montana.edu>

extern "C" int printf (const char *, ...);

class samp {
        int a;
public:
        samp(int n) { a = n; }
        int get_a() { return a; }
};

int main() {
        samp ob[4] [2] = {  // Generated Error
                1, 2,
                3, 4,
                5, 6,
                7, 8,
        };
        int i;

	if (ob[0][0].get_a() == 1 && ob[0][1].get_a() == 2
	    && ob[1][0].get_a() == 3 && ob[1][1].get_a() == 4
	    && ob[2][0].get_a() == 5 && ob[2][1].get_a() == 6
	    && ob[3][0].get_a() == 7 && ob[3][1].get_a() == 8)
	  printf ("PASS\n");
	else
	  { printf ("FAIL\n"); return 1; }

        return 0;
}
