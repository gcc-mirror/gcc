void abort (void);
void exit (int);

int
main(void)
{
  double x,y=0.5;
  x=y/0.2;
  if(x!=x)
    abort();
  exit(0);
}
