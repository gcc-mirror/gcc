// Build don't link: 
// GROUPS passed ARM-compliance
// arm file
// Message-Id: <9211231051.AA11287@us-es.sel.de>
// From: dcb@us-es.sel.de
// Subject: ARM page 87
// Date: Mon, 23 Nov 92 11:51:38 +0100


int main() {
        int a;

        switch (a) {
        case 1:
                int v2 = 3;// ERROR -    crosses.*
        case 2:// ERROR -  jump.*
                if (v2 == 7)    // error not flagged by 2.3.1
                        ;
        }

        return 0;
}

