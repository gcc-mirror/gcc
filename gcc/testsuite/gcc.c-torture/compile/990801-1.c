/* { dg-additional-options "-std=gnu17" } */

extern int getch();
extern int class();

int
token()
{
    int state = 1;

    while (1) {
	int c=0;
	c = getch();
	switch (state) {
	case 1: break;
	case 4: break;
	case 5: break;
	case 6: 
            {
	        switch (class(c)) {
	        default: break;
	        }
	    } break;
	case 7:	break;
	}
    }
}

