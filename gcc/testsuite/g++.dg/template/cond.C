// PR c++/8080

// Bug: the transformation in finish_while_stmt_cond produced something
// that tsubst_expr handled badly.  Fixed by not doing the transformation
// while parsing a template.

class TObject {};

struct TIter {
  TObject           *operator()();
};


template<class T>
void get_root_object(TIter& iobj) {
  while ( TObject* pnew_obj = iobj() )
    ;
}

void foo(TIter& iobj)
{
  get_root_object<int>(iobj);
}
