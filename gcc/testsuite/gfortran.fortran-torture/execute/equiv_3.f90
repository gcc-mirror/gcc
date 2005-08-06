      subroutine test1
      type t
      sequence
      character(8) c
      end type t
      type(t) :: tc, td
      equivalence (tc, td)
      tc%c='abcdefgh'
      if (tc%c.ne.'abcdefgh'.or.td%c(1:1).ne.'a') call abort
      end subroutine test1
      program main
      call test1
      end program main
