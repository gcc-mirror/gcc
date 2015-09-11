// PR debug/58315
// { dg-options "-O -g -fdump-tree-einline" }
// { dg-final { scan-tree-dump-not "DEBUG <L0>" "einline" } }

// We used to emit useless NOTE_INSN_DELETED_DEBUG_LABELs for the
// artificial cdtor_label.

struct A
{
  ~A() {}
};

struct B: A {};

int main()
{
  A a;
}
