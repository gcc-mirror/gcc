/* PR c/14092
 * Origin: bonzini@gnu.org
 * rejects-valid
 */
/* { dg-do compile } */

typedef struct _PLCI {
  unsigned char x;
  unsigned char buf[1];
} PLCI;


void nl_ind(PLCI * plci)
{
  plci->x = -((int)(plci->buf)) & 3;
}

