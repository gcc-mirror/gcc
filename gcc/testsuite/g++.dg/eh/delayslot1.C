// PR target/12301
// Origin: Colin Hirsch <gcc@cohi.at>
// Testcase by Christian Ehrhardt <ehrhardt@mathematik.uni-ulm.de>

// This used to fail on SPARC because the reorg pass moved an insn
// across a function call that can throw internally, in order to put
// it in a delay slot.

// { dg-do run }
// { dg-options "-O" }

struct S{
  char *c;
  char data[100];
  S () : c (data) {};
  S (const S& s) {
    c = data;
    data[0] = s.c[0];
  }
};

S real_cast ()
{
  throw 3;  
}

S cast_helper(S& debug)
{
  try {
    return real_cast();
  }
  catch (int e) {
    throw debug;
  }
}

int main()
{
  S tmp;

  try {
    cast_helper (tmp);
  }                                        
  catch (S& e) {}

  return 0;
}
