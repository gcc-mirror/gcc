// Build don't link:

// Simplified from bug report by Trevor Taylor <ttaylor@powerup.com.au>

struct T {
  int operator()(int) { } // ERROR - candidate
};

int main() {
  T()(); // ERROR - no such operator
}
