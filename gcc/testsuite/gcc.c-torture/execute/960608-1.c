void abort (void);
void exit (int);

typedef struct
{
  unsigned char a  : 2;
  unsigned char b  : 3;
  unsigned char c  : 1;
  unsigned char d  : 1;
  unsigned char e  : 1;
} a_struct;

int
foo (flags)
     a_struct *flags;
{
  return (flags->c != 0
	  || flags->d != 1
	  || flags->e != 1
	  || flags->a != 2
	  || flags->b != 3);
}

int
main (void)
{
  a_struct flags;

  flags.c  = 0;
  flags.d  = 1;
  flags.e  = 1;
  flags.a  = 2;
  flags.b  = 3;

  if (foo (&flags) != 0)
    abort ();
  else
    exit (0);
}
