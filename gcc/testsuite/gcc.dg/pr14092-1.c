/* PR c/14092
 * Origin: bonzini@gnu.org
 * rejects-valid
 */
/* { dg-do compile } */

/* Define this so that we are more portable.  The testcase in the
   PR failed on 64-bit hosts.  */
typedef int __attribute__ ((mode (__pointer__))) intptr_t;

typedef struct _PLCI {
  unsigned char x;
  unsigned char buf[1];
} PLCI;

void nl_ind(PLCI * plci)
{
  plci->x = -((intptr_t)(plci->buf)) & 3;
}

