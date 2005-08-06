      subroutine test1
      character*8 c
      character*1 d, f
      dimension d(2), f(2)
      character*4 e
      equivalence (c(1:1), d(1)), (c(3:5), e(2:4)), (c(6:6), f(2))
      c='abcdefgh'
      if (c.ne.'abcdefgh'.or.d(1).ne.'a'.or.d(2).ne.'b') call abort
      if (e.ne.'bcde'.or.f(1).ne.'e'.or.f(2).ne.'f') call abort
      end subroutine test1
      subroutine test2
      equivalence (c(1:1), d(1)), (c(3:5), e(2:4)), (c(6:6), f(2))
      character*8 c
      character*1 d, f
      dimension d(2), f(2)
      character*4 e
      c='abcdefgh'
      if (c.ne.'abcdefgh'.or.d(1).ne.'a'.or.d(2).ne.'b') call abort
      if (e.ne.'bcde'.or.f(1).ne.'e'.or.f(2).ne.'f') call abort
      end subroutine test2
      subroutine test3
      character*8 c
      character*1 d, f
      character*4 e
      equivalence (c(1:1), d(1)), (c(3:5), e(2:4)), (c(6:6), f(2))
      dimension d(2), f(2)
      c='abcdefgh'
      if (c.ne.'abcdefgh'.or.d(1).ne.'a'.or.d(2).ne.'b') call abort
      if (e.ne.'bcde'.or.f(1).ne.'e'.or.f(2).ne.'f') call abort
      end subroutine test3
      subroutine test4
      dimension d(2), f(2)
      equivalence (c(1:1), d(1)), (c(3:5), e(2:4)), (c(6:6), f(2))
      character*8 c
      character*1 d, f
      character*4 e
      c='abcdefgh'
      if (c.ne.'abcdefgh'.or.d(1).ne.'a'.or.d(2).ne.'b') call abort
      if (e.ne.'bcde'.or.f(1).ne.'e'.or.f(2).ne.'f') call abort
      end subroutine test4
      program main
      call test1
      call test2
      call test3
      call test4
      end program main
