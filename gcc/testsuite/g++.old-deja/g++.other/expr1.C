// Build don't link:

// crash test - XFAIL *-*-*

// Simplified from bug report by Trevor Taylor <ttaylor@powerup.com.au>

struct T {
  int operator()(int) { }
};

int main() {
  T()(); // ERROR - no such operator
}
