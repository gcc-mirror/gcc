/* Test for reload failing to eliminate from argp to sp.  */
/* { dg-do run { target i?86-*-* } } */
/* { dg-skip-if "" { i?86-*-* } { "-m64" "-fpic" "-fPIC" } { "" } } */
/* { dg-options "-O2 -fomit-frame-pointer" } */

static int ustrsize (const char *s);
static int (*ucwidth) (int c);
static int (*ugetxc) (const char **s);
static int (*usetc) (char *s, int c);

char *ustrzcat(char *dest, int size, const char *src)
{
   int pos = ustrsize(dest);
   int c;

   size -= pos + ucwidth(0);

   while ((c = ugetxc(&src)) != 0) {
      size -= ucwidth(c);
      if (size < 0)
         break;

      pos += usetc(dest+pos, c);
   }

   usetc(dest+pos, 0);

   return dest;
}

static int __attribute__((noinline))
ustrsize (const char *s)
{
  return 0;
}

static int
ucwidth_ (int c)
{
  return 1;
}

static int
ugetxc_ (const char **s)
{
  return '\0';
}

static int
usetc_ (char *s, int c)
{
  return 1;
}

int
main()
{
  ucwidth = ucwidth_;
  ugetxc = ugetxc_;
  usetc = usetc_;
  
  /* ??? It is impossible to explicitly modify the hard frame pointer.
     This will run afoul of code in flow.c that declines to mark regs
     in eliminate_regs in regs_ever_used.  Apparently, we have to wait
     for reload to decide that it won't need a frame pointer before a
     variable can be allocated to %ebp.

     So save, restore, and clobber %ebp by hand.  */

  asm ("pushl %%ebp\n\t"
       "movl $-1, %%ebp\n\t"
       "pushl $0\n\t"
       "pushl $0\n\t"
       "pushl $0\n\t"
       "call %P0\n\t"
       "addl $12, %%esp\n\t"
       "popl %%ebp"
       : : "i"(ustrzcat) : "memory" );

  return 0;
}
