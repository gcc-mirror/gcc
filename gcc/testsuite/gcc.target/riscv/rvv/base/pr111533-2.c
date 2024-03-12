/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -O2" } */

#include <string.h>

/* Return the number of DNS hierarchy levels in the name. */
int
test (const char *name) {
	int i, len, count;

	len = strlen(name);
	for (i = 0, count = 0; i < len; i++) {
		/* XXX need to check for \. or use named's nlabels(). */
		if (name[i] == '.')
			count++;
	}

	/* don't count initial wildcard */
	if (name[0] == '*')
		if (count)
			count--;

	/* don't count the null label for root. */
	/* if terminating '.' not found, must adjust */
	/* count to include last label */
	if (len > 0 && name[len-1] != '.')
		count++;
	return (count);
}
