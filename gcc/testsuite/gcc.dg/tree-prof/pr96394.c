/* PR ipa/96394 */
/* { dg-options "-O2" } */

typedef struct _entry {
    int has_next;
    int next_ix;
    int count;
} entry;

extern entry table[];

void *
__attribute__((noipa))
PyErr_Format(entry * e){ return 0; }

void ae(entry *);
int h(entry *);
int ap(entry *);
int ag(entry *);

int ag(entry *j) {
  if (j->has_next)
    h(&table[j->next_ix]);
  return 0;
}
static int ai(entry *j, int k(entry *), int l, int m) {
  int am = 1;
  int ab;

  /* k is either 'h' or 'ap': 50%/50% */
  ab = k(j);

  /* loop never gets executed on real data */
  for (; j->count >= 2; am += 2)
    if (l) {
      entry *i = &table[am + m];
      PyErr_Format(i);
    }
  return ab;
}
void
__attribute__((noipa))
bug() {
  h(table);
  h(table);
}
int h(entry *j) { return ai(j, ap, 4, 5); }
int ap(entry *j) { return ai(j, ag, 14, 4); }

int main(void)
{
    bug();
}

entry table[2] = {
    { .has_next = 1
    , .next_ix  = 1
    , .count    = 0
    },
    { .has_next = 0
    , .next_ix  = 0
    , .count    = 0
    },
};
