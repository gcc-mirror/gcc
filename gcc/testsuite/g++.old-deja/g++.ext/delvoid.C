// { dg-do run  }
// { dg-options "-w" }
// Jason Merrill <jason@redhat.com>
// Test for deleting a void pointer, which the standard says is undefined,
// but which is used by several free C++ programs.

void *p;

int main ()
{
  delete [] p;
}
