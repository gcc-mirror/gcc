// Build don't link: 

void * foo () {
  typedef int * ip;
  return new ip;
}
