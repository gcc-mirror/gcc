// { dg-do assemble  }

void * foo () {
  typedef int * ip;
  return new ip;
}
