/* { dg-do compile } */

typedef void * gzFile;
typedef struct
{
  int mode;
  int direct;
  int seek;
  int err;
  char *msg;
}
gz_state;

void gz_error (gz_state *state, int err, char *msg);

static void
gz_reset (gz_state *state)
{
  if (state->mode == 7247)
    {
      state->direct = 1;
    }
  state->seek = 0;
  gz_error (state, 0, 0);
}

int
gzbuffer (void *file, int size)
{
  gz_state *state;
  gz_reset (state);
}

void gz_error (gz_state *state, int err, char *msg)
{
  if (state->msg != 0)
    {
      if (state->err != -4)
	foo (state->msg);
    }
  if (msg == 0)
    return;
  bar (state->msg, msg);
}
