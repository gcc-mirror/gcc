#include "analyzer-decls.h"

typedef void (*free_func)(void *opaque, void *address);

typedef struct z_stream_s {
  struct internal_state *state;
  free_func zfree;
  void *opaque;
} z_stream;

struct internal_state {
  z_stream *strm;
  int status;
  unsigned char *pending_buf;
  unsigned char *window;
  unsigned short *prev;
  unsigned short *head;
};

int deflateEnd(z_stream *strm)
{
  int status;

  __analyzer_dump_exploded_nodes (0); /* { dg-warning "1 processed enode" } */

  if (strm == 0 || strm->state == 0)
    return (-2);

  __analyzer_dump_exploded_nodes (0); /* { dg-warning "1 processed enode" } */

  status = strm->state->status;
  if (status != 42 && status != 113 && status != 666) {
    return (-2);
  }

  __analyzer_dump_exploded_nodes (0); /* { dg-warning "1 processed enode" } */

  if (strm->state->pending_buf)
    (*(strm->zfree))(strm->opaque, (void *)(strm->state->pending_buf));

  __analyzer_dump_exploded_nodes (0); /* { dg-warning "1 processed enode" } */

  if (strm->state->head)
      (*(strm->zfree))(strm->opaque, (void *)(strm->state->head));

  __analyzer_dump_exploded_nodes (0); /* { dg-warning "1 processed enode" } */

  if (strm->state->prev)
    (*(strm->zfree))(strm->opaque, (void *)(strm->state->prev));

  __analyzer_dump_exploded_nodes (0); /* { dg-warning "1 processed enode" } */

  if (strm->state->window)
    (*(strm->zfree))(strm->opaque, (void *)(strm->state->window));

  __analyzer_dump_exploded_nodes (0); /* { dg-warning "1 processed enode" } */

  (*(strm->zfree))(strm->opaque, (void *)(strm->state));
  strm->state = 0;

  __analyzer_dump_exploded_nodes (0); /* { dg-warning "1 processed enode" } */

  return status == 113 ? (-3) : 0;
}
