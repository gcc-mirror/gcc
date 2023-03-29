/* { dg-additional-options "-fno-analyzer-call-summaries" } */
/* { dg-additional-options "-Wno-analyzer-too-complex" } */

typedef __SIZE_TYPE__ size_t;
#define NULL ((void *)0)

/* data structures */

struct screen_s {
	size_t rows;
	size_t cols;
	char **data;
};

struct context_s {
	struct screen_s *scr;
};

/* global context variable */

static struct context_s *ctx;

/* prototypes */

struct screen_s *screen_create(size_t cols, size_t rows);
void screen_destroy(struct screen_s *scr);
void resize_screen(size_t cols, size_t rows);

/* functions */

struct screen_s *screen_create(size_t cols, size_t rows)
{
	struct screen_s *result = NULL;

	result = __builtin_calloc(1, sizeof(*result));
	if (!result)
		return NULL;

	result->cols = cols;
	result->rows = rows;

	/* make one allocation which will be accessed like a 2D array */
	result->data = __builtin_calloc(rows, sizeof(result->data) + sizeof(*result->data) * cols);
	if (!result->data) {
		__builtin_free(result);
		return NULL;
	}

	/* obtain pointer to start of data area */
	char *ptr = (char *)(result->data + rows);

	/* setup pointers for each row of data to allow 2D array access */
	for (size_t row = 0; row < rows; row++)
		result->data[row] = (ptr + row * cols);
	/* array can now be accessed like data[row][col] */

	return result;
}

void screen_destroy(struct screen_s *scr)
{
	if (!scr)
		return;

	__builtin_free(scr->data);

	scr->data = NULL;
	scr->rows = 0;
	scr->cols = 0;

	__builtin_free(scr); /* { dg-bogus "leak" } */
}

void resize_screen(size_t cols, size_t rows)
{
	/* create a new screen */
	struct screen_s *new_scr = NULL;
	new_scr = screen_create(cols, rows); /* { dg-bogus "leak" } */
	if (!new_scr) {
		return;
	}

	/* swap the old screen with the new one */
	struct screen_s *old_scr = ctx->scr;
	ctx->scr = new_scr;

	/* omitted: copy the old screen contents to the new screen */

	/* free the old screen */
	screen_destroy(old_scr);
}

int main(void)
{
	ctx = __builtin_calloc(1, sizeof(*ctx));
	if (!ctx)
		__builtin_abort();

	ctx->scr = screen_create(80, 25); /* { dg-bogus "leak" } */
	resize_screen(100, 20);

	/* tidy up and quit */
	screen_destroy(ctx->scr);
	__builtin_free(ctx);
	ctx = NULL;
	return 0;
}
