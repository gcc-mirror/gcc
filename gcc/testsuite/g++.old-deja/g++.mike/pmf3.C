struct Fooey {
  void f(char* pX);
  void f(int in);
  void f(float fx);
  void h(double dx);
};

void Fooey::f(char*) { }	// ERROR - candidate
void Fooey::f(int) { }		// ERROR - candidate
void Fooey::f(float) { }	// ERROR - candidate
void Fooey::h(double zahl) { }

int main() {
  Fooey Blah;
  void (Fooey::*pointer)(double);
  pointer = &Fooey::f;		// ERROR - don't call Fooey::h
  (Blah.*pointer)(42.5);
  return 0;
}
