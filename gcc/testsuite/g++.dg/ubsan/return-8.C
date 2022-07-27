// { dg-additional-options "-fsanitize=undefined -fsanitize-undefined-trap-on-error" }

bool b;

int f() {
  if (b) return 42;
}			// { dg-warning "-Wreturn-type" }

int main() { f(); }
