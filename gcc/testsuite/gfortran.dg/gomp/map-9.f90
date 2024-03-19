! { dg-additional-options "-fdump-tree-omplower" }

! PR fortran/108545

! { dg-final { scan-tree-dump "#pragma omp target enter data map\\(struct:x \\\[len: 1\\\]\\) map\\(to:x\.a \\\[pointer set, len: \[0-9\]+\\\]\\) map\\(to:MEM <integer\\(kind=4\\)\\\[0:\\\]> \\\[\\(integer\\(kind=4\\)\\\[0:\\\] \\*\\)_\[0-9\]+] \\\[len: _\[0-9\]+\\\]\\) map\\(attach:x\.a\.data \\\[bias: 0\\\]\\)" "omplower" } }

program p
   type t
      integer, pointer :: a(:)
   end type
   type(t), volatile :: x
   !$omp target enter data map(to: x%a)
end
