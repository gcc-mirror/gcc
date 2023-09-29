#include <stdlib.h>
#include <string.h>

struct game_state {
	const char *word;
	char       *word_state;
};

const char *const teststr = "test string";

static struct game_state *
game_new(void)
{
	struct game_state tmp = {0};
	struct game_state *rval = NULL;
	size_t wordlen;

	tmp.word = teststr;
	wordlen = strlen(tmp.word);
	if ((tmp.word_state = (char *) malloc(wordlen+1)) == NULL)
		goto err;
	if ((rval = (struct game_state *) malloc(sizeof(*rval))) == NULL)
		goto err;
	memcpy(rval, &tmp, sizeof(*rval));

	return (rval);
err:
	free(tmp.word_state);
	free(rval);
	return (NULL);
} /* { dg-bogus "leak" } */

static void
game_free(struct game_state *game)
{
	if (game == NULL)
		return;
	free(game->word_state);
	free(game);
}

int
main(void)
{
	struct game_state *game;
	if ((game = game_new()) == NULL)
		exit(1);
	game_free(game);
	exit(0);
}
