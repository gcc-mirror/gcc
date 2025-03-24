! { dg-additional-options "-fdump-tree-omplower" }

subroutine sub1 (a1, a2, a3, a4)
   use iso_c_binding
   implicit none

   ! The following definitions are in omp_lib, which cannot be included
   ! in gcc/testsuite/
   integer, parameter :: omp_interop_kind = c_intptr_t

   integer(omp_interop_kind) :: a1  ! by ref
   integer(omp_interop_kind), optional :: a2 ! as pointer
   integer(omp_interop_kind), allocatable :: a3 ! ref to pointer
   integer(omp_interop_kind), value :: a4
   integer(omp_interop_kind) :: b

   !$omp interop init(target : a1, a2, a3, a4, b)
   ! { dg-final { scan-tree-dump-times "void \\* interopobjs\.\[0-9\]+\\\[5\\\];\[\r\n ]*integer\\(kind=4\\) tgt_tgtsync\.\[0-9\]+\\\[5\\\];\[\r\n ]*integer\\(kind=\[48\]\\) \\* & a3\.\[0-9\]+;\[\r\n ]*integer\\(kind=\[48\]\\) \\* D\.\[0-9\]+;\[\r\n ]*integer\\(kind=\[48\]\\) \\* a2\.\[0-9\]+;\[\r\n ]*integer\\(kind=\[48\]\\) & a1\.\[0-9\]+;\[\r\n ]*interopobjs\.\[0-9\]+\\\[0\\\] = &b;\[\r\n ]*tgt_tgtsync\.\[0-9\]+\\\[0\\\] = 1;\[\r\n ]*interopobjs\.\[0-9\]+\\\[1\\\] = &a4;\[\r\n ]*tgt_tgtsync\.\[0-9\]+\\\[1\\\] = 1;\[\r\n ]*a3\.\[0-9\]+ = a3;\[\r\n ]*D\.\[0-9\]+ = \\*a3\.\[0-9\]+;\[\r\n ]*interopobjs\.\[0-9\]+\\\[2\\\] = D\.\[0-9\]+;\[\r\n ]*tgt_tgtsync\.\[0-9\]+\\\[2\\\] = 1;\[\r\n ]*a2\.\[0-9\]+ = a2;\[\r\n ]*interopobjs\.\[0-9\]+\\\[3\\\] = a2\.\[0-9\]+;\[\r\n ]*tgt_tgtsync\.\[0-9\]+\\\[3\\\] = 1;\[\r\n ]*a1\.\[0-9\]+ = a1;\[\r\n ]*interopobjs\.\[0-9\]+\\\[4\\\] = a1\.\[0-9\]+;\[\r\n ]*tgt_tgtsync\.\[0-9\]+\\\[4\\\] = 1;\[\r\n ]*__builtin_GOMP_interop \\(-5, 5, &interopobjs\.\[0-9\]+, &tgt_tgtsync\.\[0-9\]+, 0B, 0, 0B, 0, 0B, 0, 0B\\);" 1 "omplower" } }

   !$omp interop use(a1, a2, a3, a4, b)
   ! { dg-final { scan-tree-dump-times "void \\* interopobjs\.\[0-9\]+\\\[5\\\];\[\r\n ]*integer\\(kind=\[48\]\\) b\.\[0-9\]+;\[\r\n ]*void \\* b\.\[0-9\]+;\[\r\n ]*integer\\(kind=\[48\]\\) a4\.\[0-9\]+;\[\r\n ]*void \\* a4\.\[0-9\]+;\[\r\n ]*integer\\(kind=\[48\]\\) \\* & a3\.\[0-9\]+;\[\r\n ]*integer\\(kind=\[48\]\\) \\* D\.\[0-9\]+;\[\r\n ]*integer\\(kind=\[48\]\\) D\.\[0-9\]+;\[\r\n ]*void \\* D\.\[0-9\]+;\[\r\n ]*integer\\(kind=\[48\]\\) \\* a2\.\[0-9\]+;\[\r\n ]*integer\\(kind=\[48\]\\) D\.\[0-9\]+;\[\r\n ]*void \\* D\.\[0-9\]+;\[\r\n ]*integer\\(kind=\[48\]\\) & a1\.\[0-9\]+;\[\r\n ]*integer\\(kind=\[48\]\\) D\.\[0-9\]+;\[\r\n ]*void \\* D\.\[0-9\]+;\[\r\n ]*b\.\[0-9\]+ = b;\[\r\n ]*b\.\[0-9\]+ = \\(void \\*\\) b\.\[0-9\]+;\[\r\n ]*interopobjs\.\[0-9\]+\\\[0\\\] = b\.\[0-9\]+;\[\r\n ]*a4\.\[0-9\]+ = a4;\[\r\n ]*a4\.\[0-9\]+ = \\(void \\*\\) a4\.\[0-9\]+;\[\r\n ]*interopobjs\.\[0-9\]+\\\[1\\\] = a4\.\[0-9\]+;\[\r\n ]*a3\.\[0-9\]+ = a3;\[\r\n ]*D\.\[0-9\]+ = \\*a3\.\[0-9\]+;\[\r\n ]*D\.\[0-9\]+ = \\*D\.\[0-9\]+;\[\r\n ]*D\.\[0-9\]+ = \\(void \\*\\) D\.\[0-9\]+;\[\r\n ]*interopobjs\.\[0-9\]+\\\[2\\\] = D\.\[0-9\]+;\[\r\n ]*a2\.\[0-9\]+ = a2;\[\r\n ]*D\.\[0-9\]+ = \\*a2\.\[0-9\]+;\[\r\n ]*D\.\[0-9\]+ = \\(void \\*\\) D\.\[0-9\]+;\[\r\n ]*interopobjs\.\[0-9\]+\\\[3\\\] = D\.\[0-9\]+;\[\r\n ]*a1\.\[0-9\]+ = a1;\[\r\n ]*D\.\[0-9\]+ = \\*a1\.\[0-9\]+;\[\r\n ]*D\.\[0-9\]+ = \\(void \\*\\) D\.\[0-9\]+;\[\r\n ]*interopobjs\.\[0-9\]+\\\[4\\\] = D\.\[0-9\]+;\[\r\n ]*__builtin_GOMP_interop \\(-5, 0, 0B, 0B, 0B, 5, &interopobjs\.\[0-9\]+, 0, 0B, 0, 0B\\);" 1 "omplower" } }

   !$omp interop destroy(a1, a2, a3, a4, b)
   ! { dg-final { scan-tree-dump-times "void \\* interopobjs\.\[0-9\]+\\\[5\\\];\[\r\n ]*integer\\(kind=\[48\]\\) \\* & a3\.\[0-9\]+;\[\r\n ]*integer\\(kind=\[48\]\\) \\* D\.\[0-9\]+;\[\r\n ]*integer\\(kind=\[48\]\\) \\* a2\.\[0-9\]+;\[\r\n ]*integer\\(kind=\[48\]\\) & a1\.\[0-9\]+;\[\r\n ]*interopobjs\.\[0-9\]+\\\[0\\\] = &b;\[\r\n ]*interopobjs\.\[0-9\]+\\\[1\\\] = &a4;\[\r\n ]*a3\.\[0-9\]+ = a3;\[\r\n ]*D\.\[0-9\]+ = \\*a3\.\[0-9\]+;\[\r\n ]*interopobjs\.\[0-9\]+\\\[2\\\] = D\.\[0-9\]+;\[\r\n ]*a2\.\[0-9\]+ = a2;\[\r\n ]*interopobjs\.\[0-9\]+\\\[3\\\] = a2\.\[0-9\]+;\[\r\n ]*a1\.\[0-9\]+ = a1;\[\r\n ]*interopobjs\.\[0-9\]+\\\[4\\\] = a1\.\[0-9\]+;\[\r\n ]*__builtin_GOMP_interop \\(-5, 0, 0B, 0B, 0B, 0, 0B, 5, &interopobjs\.\[0-9\]+, 0, 0B\\);" 1 "omplower" } }
end subroutine


