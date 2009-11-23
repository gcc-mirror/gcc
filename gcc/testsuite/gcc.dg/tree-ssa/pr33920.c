/* Testcase by Martin Michlmayr <tbm@cyrius.com> */
/* { dg-do compile } */
/* { dg-options "-O3" } */

typedef __PTRDIFF_TYPE__ intptr_t;
typedef union lispunion *object;
struct character
{
  long e;
};
extern struct symbol Cnil_body;
extern struct symbol Ct_body;
struct vector
{
  object *v_self;
};
union lispunion
{
  struct vector v;
};
void init_code ()
{
  object V659;
  object _x, _y;
  object V643;
  intptr_t V648;
  unsigned char V653;
  object V651;
  object V654;
  object V658;

T1240:
if (V648 >= (intptr_t)V651)
    goto T1243;
  V653 = ((char *) V654->v.v_self)[V648];
  V659 = (object) V654 + V653;
T1261:
  V658 =
    (object)
     V659 ? (object) & Ct_body : (object) & Cnil_body;
  if (V658 == (object) & Cnil_body)
    goto T1249;
  goto T1224;
T1249:
  V648 = (intptr_t) V648 + 1;
  goto T1240;
T1243:
  V643 = (object) & Cnil_body;
T1224:
  _y = V643;
  number_plus (_x, _y);
}
