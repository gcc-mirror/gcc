// Build don't link: 
// GROUPS passed operators
// opr-new file
// From: flisakow@cae.wisc.edu
// Date:     Thu, 1 Sep 94 18:21:09 CDT
// Subject:  g++ bug?
// Message-ID: <9409012321.AA05346@hprisc-19.cae.wisc.edu>

#include <stdio.h>


struct fcell {
        FILE *fd;
        struct fcell *next;
};


class FStack {
public:
        struct fcell *top;
        FStack() { top = NULL ; } ;
        inline void push(FILE * fd1, int line_num, char *fname = NULL) {
                struct fcell *tmp = new struct fcell;
                tmp->fd = fd1;
                tmp->next = top;
                top = tmp ;
        }
};
