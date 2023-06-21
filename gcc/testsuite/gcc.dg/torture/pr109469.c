/* { dg-do compile } */

__attribute__((returns_twice)) int foo();

struct xio myproc;
struct xio {
  void (*read_proc)();
  void (*write_proc)();
};

void dummy_write_proc() {
  switch (foo())
  default:
    myproc.read_proc = myproc.write_proc = dummy_write_proc;
}
