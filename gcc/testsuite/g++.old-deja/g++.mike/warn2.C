// Build don't link:
// Special g++ Options: -Wall

enum Boolean {
  Ok = 0,
  NotOk = 1,
};

enum OpResult {
  Succeeded = 0,
  TempFail = 1,
  PermFail = 2,
};

OpResult fn1() {
  return TempFail;
}

extern void foo();

int
main () {
  if (fn1() == Ok) {	// WARNING - 
    foo();
  }
}
