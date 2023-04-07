/* { dg-additional-options "-Wno-analyzer-too-complex" } */

#define NULL ((void *)0)

#define LEN 64

char **
epystr_explode(const char *delim, char *str)
{
	char **out = NULL;
	int i;

	if (str == NULL || delim == NULL)
		return NULL;

	out = __builtin_malloc(LEN * sizeof(char *));
	if (out == NULL)
		return NULL;

	for (i = 0; i < LEN; i++) {
		out[i] = __builtin_strdup("bla");
		if (out[i] == NULL) /* { dg-bogus "leak" } */
			goto freem;
	}
	return out;

freem:
	while (--i >= 0)
		__builtin_free(out[i]);
	__builtin_free(out);
	return NULL;
}
