/* PR c/5354 */
/* Verify that GCC preserves relevant stack slots.  */

extern void abort(void);
extern void exit(int);

struct large { int x, y[9]; };

int main()
{
  int fixed;

  fixed = ({ int temp1 = 2; temp1; }) - ({ int temp2 = 1; temp2; });
  if (fixed != 1)
    abort();

  fixed = ({ struct large temp3; temp3.x = 2; temp3; }).x
	  - ({ struct large temp4; temp4.x = 1; temp4; }).x;
  if (fixed != 1)
    abort();

  exit(0);
}
