// { dg-do run }
// { dg-shouldfail "asan" }

struct IntHolder {
  int val;
};

const IntHolder *saved;

void save(const IntHolder &holder) {
  saved = &holder;
}

int main(int argc, char *argv[]) {
  save({10});
  int x = saved->val;  // BOOM
  return x;
}

// { dg-output "ERROR: AddressSanitizer: stack-use-after-scope on address.*(\n|\r\n|\r)" }
// { dg-output "READ of size 4 at.*" }
// { dg-output ".*'<unknown>' <== Memory access at offset \[0-9\]* is inside this variable.*" }
