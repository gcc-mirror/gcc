#include "../../gcc.dg/analyzer/analyzer-decls.h"

struct arg_buf
{
  int i;
  int j;
};

/* Example of marking a function as tainted.  */

void __attribute__((tainted_args))
test_1 (int i, void *p, char *q)
{
  /* There should be a single enode,
     for the "tainted" entry to the function.  */
  __analyzer_dump_exploded_nodes (0); /* { dg-warning "1 processed enode" } */

  __analyzer_dump_state ("taint", i); /* { dg-warning "state: 'tainted'" } */
  __analyzer_dump_state ("taint", p); /* { dg-warning "state: 'tainted'" } */
  __analyzer_dump_state ("taint", q); /* { dg-warning "state: 'tainted'" } */
  __analyzer_dump_state ("taint", *q); /* { dg-warning "state: 'tainted'" } */

  struct arg_buf *args = (struct arg_buf *) p;
  __analyzer_dump_state ("taint", args->i); /* { dg-warning "state: 'tainted'" } */
  __analyzer_dump_state ("taint", args->j); /* { dg-warning "state: 'tainted'" } */  
}

/* Example of marking a callback field as tainted.  */

struct s2
{
  void (*cb) (int, void *, char *)
    __attribute__((tainted_args));
};

/* Function not marked as tainted.  */

void
test_2a (int i, void *p, char *q)
{
  /* There should be a single enode,
     for the normal entry to the function.  */
  __analyzer_dump_exploded_nodes (0); /* { dg-warning "1 processed enode" } */

  __analyzer_dump_state ("taint", i); /* { dg-warning "state: 'start'" } */
  __analyzer_dump_state ("taint", p); /* { dg-warning "state: 'start'" } */
  __analyzer_dump_state ("taint", q); /* { dg-warning "state: 'start'" } */

  struct arg_buf *args = (struct arg_buf *) p;
  __analyzer_dump_state ("taint", args->i); /* { dg-warning "state: 'start'" } */
  __analyzer_dump_state ("taint", args->j); /* { dg-warning "state: 'start'" } */  
}

/* Function referenced via t2b.cb, marked as "tainted".  */

void
test_2b (int i, void *p, char *q)
{
  /* There should be two enodes
     for the direct call, and the "tainted" entry to the function.  */
  __analyzer_dump_exploded_nodes (0); /* { dg-warning "2 processed enodes" } */
}

/* Callback used via t2c.cb, marked as "tainted".  */
void
__analyzer_test_2c (int i, void *p, char *q)
{
  /* There should be a single enode,
     for the "tainted" entry to the function.  */
  __analyzer_dump_exploded_nodes (0); /* { dg-warning "1 processed enode" } */

  __analyzer_dump_state ("taint", i); /* { dg-warning "state: 'tainted'" } */
  __analyzer_dump_state ("taint", p); /* { dg-warning "state: 'tainted'" } */
  __analyzer_dump_state ("taint", q); /* { dg-warning "state: 'tainted'" } */
}

struct s2 t2b =
{
  .cb = test_2b
};

struct s2 t2c =
{
  .cb = __analyzer_test_2c
};
