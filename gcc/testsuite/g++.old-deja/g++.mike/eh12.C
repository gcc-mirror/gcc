// Special g++ Options: -fexceptions
// excess errors test - XFAIL sparc64-*-elf arm-*-pe

class MyError { };

int main (int argc, char **argv) {
  try {
    throw MyError();
  }
  catch (MyError x) {
  }

  return 0;
}
