// { dg-do run { xfail sparc64-*-elf arm-*-pe } }
// { dg-options "-fexceptions" }

class MyError { };

int main (int argc, char **argv) {
  try {
    throw MyError();
  }
  catch (MyError x) {
  }

  return 0;
}
