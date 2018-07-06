// { dg-do assemble  }

// Simplified from bug report by Trevor Taylor <ttaylor@powerup.com.au>

struct T {
  int operator()(int) { return 0; } // { dg-message "operator|candidate expects" }
};

int main() {
  T()(); // { dg-error "match" } no such operator
  return 0;
}
