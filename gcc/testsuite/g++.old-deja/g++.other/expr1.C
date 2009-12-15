// { dg-do assemble  }

// Simplified from bug report by Trevor Taylor <ttaylor@powerup.com.au>

struct T {
  int operator()(int) { } // { dg-message "candidate is" }
};

int main() {
  T()(); // { dg-error "match" } no such operator
}
