/* { dg-do compile } */
/* { dg-options "-fno-guess-branch-probability  -fno-tree-forwprop --param max-early-inliner-iterations=10 --param=early-inlining-insns=176" } */

template < typename T > struct intrusive_ptr
{
  ~intrusive_ptr ()
  {
    delete px;
  }
  T *px;
};

struct section_info
{
  intrusive_ptr < section_info > parent;
};

struct file_info
{
  intrusive_ptr < file_info > parent;
  intrusive_ptr < section_info > switched_section;
};


void
start_file (void)
{
  intrusive_ptr < file_info > parent;
}

