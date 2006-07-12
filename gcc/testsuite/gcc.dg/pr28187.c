/* { dg-do compile } */
/* { dg-options "-O -ftree-vrp -fwrapv" } */

extern void bar(int);
void checkgroups(int last, int verbose) 
{
    int window = 0;
    int outstanding = 0;
    while (window < last || outstanding) {
	while (outstanding < 47 && window < last) {
	    if (window < last) { 
		outstanding++; 
		if (verbose)
		    bar(window);
		bar(window++);
	    }
	}
	if (outstanding > 0)
	    bar(0);
    }
}

