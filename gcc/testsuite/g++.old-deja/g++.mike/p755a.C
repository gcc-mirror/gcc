// It checks to see if you can define your own global delete operator.
// prms-id: 755

extern "C" void _exit(int);

void operator delete(void *p) throw() {
  _exit(0);
}

int main () {
  int* i = new int;
  delete i;
  return 1;
}
