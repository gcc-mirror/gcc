// Build don't link: 
// GROUPS passed old-abort
int	strcmp();

extern "C" {
      // dies in common_type, cuz the TREE_TYPE of t2 is 0, so it can't get its
      // TYPE_MAIN_VARIANT value.
      // <void_type 184510 void permanent VOID
      //  size <integer_cst 1844e0 type <integer_type 182548 int> constant permanent 0
      //    align 1 symtab 0
      //    pointer_to_this <pointer_type 1845e0>

int       strcmp(const char*, const char*);
}
