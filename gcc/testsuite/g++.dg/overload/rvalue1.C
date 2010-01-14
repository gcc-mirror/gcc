// PR c++/42655

void unused(const bool &) { }

int main() {
  volatile bool x = false;
  unused(!!x); // type of "!x" is bool
  unused(!x); // type of "!x" is bool
}

