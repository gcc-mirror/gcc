static union {
  int i;
};

int *ip;

void g() {
  ip = &i;
}
