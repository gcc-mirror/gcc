/* Function definitions that are used by multiple tests.  */

#define INITS(NAME,TYPEM)					\
void initS##NAME##1 (S##NAME##1 *p, TYPEM y)			\
{ p->a = y; }							\
void initS##NAME##2 (S##NAME##2 *p, TYPEM y)			\
{ p->a = y; p->b = y+1; }					\
void initS##NAME##3 (S##NAME##3 *p, TYPEM y)			\
{ p->a = y; p->b = y+1; p->c = y+2; }				\
void initS##NAME##4 (S##NAME##4 *p, TYPEM y)			\
{ p->a = y; p->b = y+1; p->c = y+2; p->d = y+3; }		\
void initS##NAME##5 (S##NAME##5 *p, TYPEM y)			\
{ p->a = y; p->b = y+1; p->c = y+2; p->d = y+3; p->e = y+4; }	\
void initS##NAME##6 (S##NAME##6 *p, TYPEM y)			\
{ p->a = y; p->b = y+1; p->c = y+2; p->d = y+3; p->e = y+4;	\
  p->f = y+5; }							\
void initS##NAME##7 (S##NAME##7 *p, TYPEM y)			\
{ p->a = y; p->b = y+1; p->c = y+2; p->d = y+3; p->e = y+4;	\
  p->f = y+5; p->g = y+6; }					\
void initS##NAME##8 (S##NAME##8 *p, TYPEM y)			\
{ p->a = y; p->b = y+1; p->c = y+2; p->d = y+3; p->e = y+4;	\
  p->f = y+5; p->g = y+6; p->h = y+7; }				\
void initS##NAME##9 (S##NAME##9 *p, TYPEM y)			\
{ p->a = y; p->b = y+1; p->c = y+2; p->d = y+3; p->e = y+4;	\
  p->f = y+5; p->g = y+6; p->h = y+7; p->i = y+8; }		\
void initS##NAME##10 (S##NAME##10 *p, TYPEM y)			\
{ p->a = y; p->b = y+1; p->c = y+2; p->d = y+3; p->e = y+4;	\
  p->f = y+5; p->g = y+6; p->h = y+7; p->i = y+8; p->j = y+9; }	\
void initS##NAME##11 (S##NAME##11 *p, TYPEM y)			\
{ p->a = y; p->b = y+1; p->c = y+2; p->d = y+3; p->e = y+4;	\
  p->f = y+5; p->g = y+6; p->h = y+7; p->i = y+8; p->j = y+9;	\
  p->k = y+10; }						\
void initS##NAME##12 (S##NAME##12 *p, TYPEM y)			\
{ p->a = y; p->b = y+1; p->c = y+2; p->d = y+3; p->e = y+4;	\
  p->f = y+5; p->g = y+6; p->h = y+7; p->i = y+8; p->j = y+9;	\
  p->k = y+10; p->l = y+11; }					\
void initS##NAME##13 (S##NAME##13 *p, TYPEM y)			\
{ p->a = y; p->b = y+1; p->c = y+2; p->d = y+3; p->e = y+4;	\
  p->f = y+5; p->g = y+6; p->h = y+7; p->i = y+8; p->j = y+9;	\
  p->k = y+10; p->l = y+11; p->m = y+12; }			\
void initS##NAME##14 (S##NAME##14 *p, TYPEM y)			\
{ p->a = y; p->b = y+1; p->c = y+2; p->d = y+3; p->e = y+4;	\
  p->f = y+5; p->g = y+6; p->h = y+7; p->i = y+8; p->j = y+9;	\
  p->k = y+10; p->l = y+11; p->m = y+12; p->n = y+13; }		\
void initS##NAME##15 (S##NAME##15 *p, TYPEM y)			\
{ p->a = y; p->b = y+1; p->c = y+2; p->d = y+3; p->e = y+4;	\
  p->f = y+5; p->g = y+6; p->h = y+7; p->i = y+8; p->j = y+9;	\
  p->k = y+10; p->l = y+11; p->m = y+12; p->n = y+13;		\
  p->o = y+14; }						\
void initS##NAME##16 (S##NAME##16 *p, TYPEM y)			\
{ p->a = y; p->b = y+1; p->c = y+2; p->d = y+3; p->e = y+4;	\
  p->f = y+5; p->g = y+6; p->h = y+7; p->i = y+8; p->j = y+9;	\
  p->k = y+10; p->l = y+11; p->m = y+12; p->n = y+13;		\
  p->o = y+14; p->p = y+15; }
