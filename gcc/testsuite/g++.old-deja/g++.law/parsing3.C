// Build don't link: 
// GROUPS passed parsing
// parsing folder
// From: tll@cco.caltech.edu (Tal Lewis Lancaster)
// Date:     18 Mar 1993 17:09:43 GMT
// Subject:  Re: unexpected difference between gcc and g++ (both 2.3.3)
// Message-ID: <1oaacnINNt20@gap.caltech.edu>

/* Notice that this case parses fine */
int (* volatile y)[10];

void foo2() {
        /* The parser can't handle it now */
        int (* volatile x)[10];
}
