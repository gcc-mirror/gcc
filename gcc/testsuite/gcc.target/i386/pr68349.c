/* PR target/68483 */
/* { dg-do compile } */
/* { dg-options "-std=gnu17 -O2" } */

int a, b;
unsigned long my_strlen();
typedef struct sHyphenNode {
  char sepcnts[0];
  struct sHyphenNode *Daughters[];
} * PHyphenNode;
int GetIndex();
PHyphenNode c;
void DoHyphens_Field_1() {
  char d[300], e[300];
  int z, f, l = my_strlen();
  for (; z;)
    ;
  for (; l; z++) {
    f = z;
    for (; f < l; f++) {
      c = c->Daughters[GetIndex(d[f])];
      a = 0;
      for (; a <= f - z; a++)
	if (e[z + a])
	  e[z] = c->sepcnts[a];
    }
  }
  if (e[z])
    b = 1;
}
