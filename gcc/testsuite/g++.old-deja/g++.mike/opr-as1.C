// Shows a problem with the default op= not being an implementation...

class C {
  int i;
};

C a, b;

int main() {
  a = b;
}
