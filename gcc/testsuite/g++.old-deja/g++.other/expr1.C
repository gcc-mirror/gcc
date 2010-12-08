// { dg-do assemble  }

// Simplified from bug report by Trevor Taylor <ttaylor@powerup.com.au>

struct T {			// { dg-message "candidate" }
  int operator()(int) { } // { dg-message "operator|candidate expects" }
};

int main() {
  T()(); // { dg-error "match" } no such operator
}
