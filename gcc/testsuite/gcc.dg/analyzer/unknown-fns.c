/* Verify that the analyzer correctly purges state when it sees a call to
   an unknown function.  */

#include <stdlib.h>

/* Verify fix for false-positive when checking for CVE-2005-1689.  */

typedef struct _krb5_data {
  char *data;
} krb5_data;

extern void krb5_read_message(krb5_data *buf);

void
test_1 (krb5_data inbuf)
{
  free(inbuf.data);
  krb5_read_message(&inbuf); 
  free(inbuf.data); /* { dg-bogus "double-'free'" } */
}

/* Verify that __pure__ functions are treated as not having side-effects.  */

extern int called_by_test_1a (void *)
  __attribute__ ((__pure__));
void test_1a (krb5_data inbuf)
{
  free (inbuf.data);
  called_by_test_1a (&inbuf);
  free (inbuf.data); /* { dg-warning "double-'free'" } */
}

/* Verify that global pointers can be affected by an unknown function.  */

void *global_ptr;
extern void unknown_side_effects (void);

void test_2 (void)
{
  free (global_ptr);
  unknown_side_effects ();
  free (global_ptr);
}

extern void called_by_test_3 (void *);

void test_3a (void)
{
  void *ptr = malloc (1024);
  called_by_test_3 (ptr);
}  /* { dg-bogus "leak" } */

void test_3b (void)
{
  krb5_data k;
  k.data = malloc (1024);
  called_by_test_3 (&k);
} /* { dg-bogus "leak" } */

/* Verify that we traverse the graph of regions that are reachable from
   the call.  */

struct foo
{
  struct foo *next;
  int *ptr;
};

/* First, without a call to an unknown function.  */

void test_4a (void)
{
  struct foo node_a;
  struct foo node_b;
  node_a.next = &node_b;
  node_b.ptr = malloc (sizeof (int));
  global_ptr = &node_a;
  *node_b.ptr = 42; /* { dg-warning "possibly-NULL" "possibly-NULL" } */
  /* { dg-warning "leak" "leak" { target *-*-* } .-1 } */
  /* FIXME: the above leak report is correct, but is reported at the wrong
     location.  */
} /* { dg-warning "leak" } */

/* With a call to an unknown function.  */

void test_4b (void)
{
  struct foo node_a;
  struct foo node_b;
  node_a.next = &node_b;
  node_b.ptr = malloc (sizeof (int));
  global_ptr = &node_a;
  unknown_side_effects (); /* everything potentially visible through global_ptr.  */
  *node_b.ptr = 42; /* { dg-bogus "possibly-NULL" } */
} /* { dg-bogus "leak" } */

extern void called_by_test_5 (const char *);
void test_5 (void)
{
  called_by_test_5 ("???");
}

extern void called_by_test_6 (const struct foo *);
void test_6 (void)
{
  struct foo node;
  node.next = NULL;
  node.ptr = malloc (sizeof (int));

  /* This is a const ptr, but struct foo's ptr is non-const,
     so we ought to assume it could be written to.  */
  called_by_test_6 (&node);
} /* { dg-bogus "leak" } */

/* TODO: things reachable from "outside" i.e. by params to caller to entrypoint.  */
