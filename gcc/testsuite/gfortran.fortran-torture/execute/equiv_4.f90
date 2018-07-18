      subroutine test1
      character*8 c
      character*2 d, f
      dimension d(2), f(2)
      character*4 e
      equivalence (c(1:1), d(1)(2:)), (c(3:5), e(2:4))
      equivalence (c(6:6), f(2)(:))
      d(1)='AB'
      c='abcdefgh'
      if (c.ne.'abcdefgh'.or.d(1).ne.'Aa'.or.d(2).ne.'bc') STOP 1
      if (e.ne.'bcde'.or.f(1).ne.'de'.or.f(2).ne.'fg') STOP 2
      end subroutine test1
      subroutine test2
      equivalence (c(1:1), d(1)(2:2)), (c(3:5), e(2:4))
      equivalence (c(6:6), f(2)(1:))
      character*8 c
      character*2 d, f
      dimension d(2), f(2)
      character*4 e
      d(1)='AB'
      c='abcdefgh'
      if (c.ne.'abcdefgh'.or.d(1).ne.'Aa'.or.d(2).ne.'bc') STOP 3
      if (e.ne.'bcde'.or.f(1).ne.'de'.or.f(2).ne.'fg') STOP 4
      end subroutine test2
      subroutine test3
      character*8 c
      character*2 d, f
      character*4 e
      equivalence (c(1:1), d(1)(2:)), (c(3:5), e(2:4))
      equivalence (c(6:6), f(2)(:1))
      dimension d(2), f(2)
      d(1)='AB'
      c='abcdefgh'
      if (c.ne.'abcdefgh'.or.d(1).ne.'Aa'.or.d(2).ne.'bc') STOP 5
      if (e.ne.'bcde'.or.f(1).ne.'de'.or.f(2).ne.'fg') STOP 6
      end subroutine test3
      subroutine test4
      dimension d(2), f(2)
      equivalence (c(1:1), d(1)(2:2)), (c(3:5), e(2:4))
      equivalence (c(6:6), f(2)(1:2))
      character*8 c
      character*2 d, f
      character*4 e
      d(1)='AB'
      c='abcdefgh'
      if (c.ne.'abcdefgh'.or.d(1).ne.'Aa'.or.d(2).ne.'bc') STOP 7
      if (e.ne.'bcde'.or.f(1).ne.'de'.or.f(2).ne.'fg') STOP 8
      end subroutine test4
      program main
      call test1
      call test2
      call test3
      call test4
      end program main
