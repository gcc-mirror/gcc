// It checks to see if you can define your own global delete operator.
// prms-id: 755

extern "C" void exit(int);

void operator delete(void *p) throw() {
  exit(0);
}

main () {
  int* i = new int;
  delete i;
  return 1;
}
