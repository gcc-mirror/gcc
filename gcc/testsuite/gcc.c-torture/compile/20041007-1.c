/* PR rtl-optimization/17027 */
/* Origin: dbk <sfc@village.uunet.be> */
/* Testcase by Christian Ehrhardt <ehrhardt@mathematik.uni-ulm.de> */

int bar(void);
void baz (void)  __attribute__ ((noreturn)); /* noreturn is required */

void foo (void) 
{
  while (bar ()) {
    switch (1) {
      default:
      baz ();
    }
  }
} 
