C PR debug/37738
C { dg-do compile }
C { dg-skip-if "DWARF-2 only" { "*-*-*" } { "*" } { "-gdwarf-2" } }
C { dg-skip-if "DWARF-2 only" { "*-*-*" } { "-g1" } { "" } }
C { dg-options "-dA" }

      subroutine a
      integer*4 a_i, c_i
      common /block/a_i, c_i
      a_i = 1
      c_i = 4
      end subroutine a
      subroutine b
      integer*4 b_i
      common /block/b_i, d_i
      b_i = 2
      d_i = 5
      end subroutine b
      subroutine c
      integer*4 a_i, c_i
      common /block/a_i, c_i
      if (a_i .ne. 2) call abort
      if (c_i .ne. 5) call abort
      end subroutine c
      program abc
      call a
      call b
      call c
      end program abc

C { dg-final { scan-assembler-times "DIE\[^\n\]*DW_TAG_common_block" 3 } }
