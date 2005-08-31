#include        <stdio.h>
#include        <stdlib.h>
extern void abort();

typedef struct foo
{
        int     uaattrid;
        char    *name;
} FOO;

FOO     Upgrade_items[] =
{
        {1, "1"},
        {2, "2"},
        {0, NULL}
};

int     *Upgd_minor_ID = 
        (int *) &((Upgrade_items + 1)->uaattrid);

int     *Upgd_minor_ID1 = 
        (int *) &((Upgrade_items)->uaattrid);

int
main(int argc, char **argv)
{
	if (*Upgd_minor_ID != 2)
	  abort();

	if (*Upgd_minor_ID1 != 1)
	  abort();
	return 0;
}
