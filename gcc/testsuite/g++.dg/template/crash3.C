struct S {
};

extern S i[];

void g (S*);

template <typename T>
void f () {
  g (&i[2]);
}

