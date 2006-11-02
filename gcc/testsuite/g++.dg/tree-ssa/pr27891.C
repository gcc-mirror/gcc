/* { dg-do compile } */
/* { dg-options "-O2" } */

int firstkey();
void DBM_error(int);

void domisc() {
    int i = 0;
    try {
	try {
	    firstkey();
	    while (1) {
		i++;
		firstkey();
	    }
	} catch (int) {
	    ;
	}
	DBM_error(i);
    } catch (int) {
	;
    }
}
