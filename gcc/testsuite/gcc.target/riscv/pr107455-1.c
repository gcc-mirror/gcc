/* { dg-do compile } */
/* { dg-additional-options "-dp" } */
/* { dg-skip-if "" { *-*-* } { "-O0" "-O1" "-O2" "-O3" "-Og" } } */


typedef struct dllist
{
  int i;
  struct dllist *ptr_to_next;
  struct dllist *ptr_to_previous;
} dllist;

int sglib_dllist_len(dllist *list) {
    int res;
    dllist *_dl_;
    int _r1_, _r2_;
    if (list== ((void *)0)) {
        res = 0;
    } else { 
        dllist *_ce_;
        dllist *_ne_;
        _r1_ = 0;
        _ce_ = list;
        while (_ce_!= ((void *)0)) {
            _ne_ = _ce_->ptr_to_previous;
            _r1_++;
            _ce_ = _ne_;
        }
        _dl_ = list->ptr_to_next;
        _r2_ = 0;
        _ce_ = _dl_;
        while (_ce_!= (void *)0) {
            _ne_ = _ce_->ptr_to_next;
            _r2_++;
            _ce_ = _ne_;
        }
        res = _r1_ + _r2_;
    }
    return res;
}


/* There was an unnecessary assignment to the return value until
   recently.  Scan for that in the resulting output.  */
/* { dg-final { scan-assembler-times "li\\ta0,0" 1 } } */

