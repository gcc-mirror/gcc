// { dg-do assemble  }

// Simplified from bug report by Trevor Taylor <ttaylor@powerup.com.au>

struct T {
  int operator()(int) { } // { dg-error "" } candidate
};

int main() {
  T()(); // { dg-error "" } no such operator
}
