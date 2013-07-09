// PR c++/57551

extern unsigned long ADDR;

unsigned long f(){
  const unsigned long* const var=&ADDR;
  const unsigned long retval=var[1];
  return retval;
}
