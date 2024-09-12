/* As pr94851-1.c, but verify that we don't get confused by a call to
   an unknown function (PR analyzer/98575).  */

/* { dg-additional-options "-O2" } */
/* { dg-skip-if "requires hosted libstdc++ for stdlib malloc" { ! hostedlib } } */

#include <stdio.h>
#include <stdlib.h>

typedef struct AMARK {
  struct AMARK *m_next;
  char m_name;
} AMARK;

struct buf {
  AMARK *b_amark;
};

struct buf *curbp;

extern void unknown_fn (void);

int pamark(void) {
  int c;

  AMARK *p = curbp->b_amark;
  AMARK *last = curbp->b_amark;

  unknown_fn ();

  c = getchar ();

  while (p != (AMARK *)NULL && p->m_name != (char)c) {
    last = p;
    p = p->m_next;
  }

  if (p != (AMARK *)NULL) {
    printf("over writing mark %c\n", c);
  } else {
    if ((p = (AMARK *)malloc(sizeof(AMARK))) == (AMARK *)NULL)
      return 0;

    p->m_next = (AMARK *)NULL;

    if (curbp->b_amark == (AMARK *)NULL)
      curbp->b_amark = p;
    else
      last->m_next = p; /* { dg-warning "dereference of NULL 'last'" "deref" } */
  }

  p->m_name = (char)c; /* { dg-bogus "leak of 'p'" "bogus leak" } */

  return 1;
}
