// { dg-lto-do link }

// Test that type of asm string is not removed.
char c;
asm("");

int main() { }
