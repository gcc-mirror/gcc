#include <stdlib.h>

void example(void) {
	int len;
	int **namelist = NULL;

	len = 2;
	namelist = malloc(len * sizeof *namelist);
	if (!namelist) return;
	namelist[0] = malloc(sizeof **namelist);
	namelist[1] = malloc(sizeof **namelist);

	while(len--) { free(namelist[len]); }
	free(namelist);
	return;
}
