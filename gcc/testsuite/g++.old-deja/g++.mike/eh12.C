// Special g++ Options: -fexceptions
// excess errors test - XFAIL a29k-*-* sparc64-*-elf sh-*-* arm-*-pe**-*

class MyError { };

int main (int argc, char **argv) {
  try {
    throw MyError();
  }
  catch (MyError x) {
  }

  return 0;
}
